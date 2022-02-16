use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::{Loc, Position, Region};
use Progress::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Either<First, Second> {
    First(First),
    Second(Second),
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
    Space(BadInputError),
    NotEndOfFile(Position),
}
pub trait SpaceProblem {
    fn space_problem(e: BadInputError, pos: Position) -> Self;
}

macro_rules! impl_space_problem {
    ($($name:ident $(< $lt:tt >)?),*) => {
        $(
            impl $(< $lt >)? SpaceProblem for $name $(< $lt >)? {
                fn space_problem(e: BadInputError, pos: Position) -> Self {
                    Self::Space(e, pos)
                }
            }
        )*
    };
}

impl_space_problem! {
    EExpect<'a>,
    EExposes,
    EExpr<'a>,
    EGenerates,
    EGeneratesWith,
    EHeader<'a>,
    EIf<'a>,
    EImports,
    EInParens<'a>,
    ELambda<'a>,
    EList<'a>,
    EPackageEntry<'a>,
    EPackages<'a>,
    EPattern<'a>,
    EProvides<'a>,
    ERecord<'a>,
    ERequires<'a>,
    EString<'a>,
    EType<'a>,
    ETypeInParens<'a>,
    ETypeRecord<'a>,
    ETypeTagUnion<'a>,
    ETypedIdent<'a>,
    EWhen<'a>,
    PInParens<'a>,
    PRecord<'a>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EHeader<'a> {
    Provides(EProvides<'a>, Position),
    Exposes(EExposes, Position),
    Imports(EImports, Position),
    Requires(ERequires<'a>, Position),
    Packages(EPackages<'a>, Position),
    Generates(EGenerates, Position),
    GeneratesWith(EGeneratesWith, Position),

    Space(BadInputError, Position),
    Start(Position),
    ModuleName(Position),
    AppName(EString<'a>, Position),
    PlatformName(EPackageName<'a>, Position),
    IndentStart(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EProvides<'a> {
    Provides(Position),
    Open(Position),
    To(Position),
    IndentProvides(Position),
    IndentTo(Position),
    IndentListStart(Position),
    IndentListEnd(Position),
    IndentPackage(Position),
    ListStart(Position),
    ListEnd(Position),
    Identifier(Position),
    Package(EPackageName<'a>, Position),
    Space(BadInputError, Position),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EExposes {
    Exposes(Position),
    Open(Position),
    IndentExposes(Position),
    IndentListStart(Position),
    IndentListEnd(Position),
    ListStart(Position),
    ListEnd(Position),
    Identifier(Position),
    Space(BadInputError, Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ERequires<'a> {
    Requires(Position),
    Open(Position),
    IndentRequires(Position),
    IndentListStart(Position),
    IndentListEnd(Position),
    ListStart(Position),
    ListEnd(Position),
    TypedIdent(ETypedIdent<'a>, Position),
    Rigid(Position),
    Space(BadInputError, Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypedIdent<'a> {
    Space(BadInputError, Position),
    HasType(Position),
    IndentHasType(Position),
    Name(Position),
    Type(EType<'a>, Position),
    IndentType(Position),
    Identifier(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPackages<'a> {
    Open(Position),
    Space(BadInputError, Position),
    Packages(Position),
    IndentPackages(Position),
    ListStart(Position),
    ListEnd(Position),
    IndentListStart(Position),
    IndentListEnd(Position),
    PackageEntry(EPackageEntry<'a>, Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPackageName<'a> {
    BadPath(EString<'a>, Position),
    Escapes(Position),
    Multiline(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPackageEntry<'a> {
    BadPackage(EPackageName<'a>, Position),
    Shorthand(Position),
    Colon(Position),
    IndentPackage(Position),
    Space(BadInputError, Position),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EImports {
    Open(Position),
    Imports(Position),
    IndentImports(Position),
    IndentListStart(Position),
    IndentListEnd(Position),
    ListStart(Position),
    ListEnd(Position),
    Identifier(Position),
    ExposingDot(Position),
    ShorthandDot(Position),
    Shorthand(Position),
    ModuleName(Position),
    Space(BadInputError, Position),
    IndentSetStart(Position),
    IndentSetEnd(Position),
    SetStart(Position),
    SetEnd(Position),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EGenerates {
    Open(Position),
    Generates(Position),
    IndentGenerates(Position),
    Identifier(Position),
    Space(BadInputError, Position),
    IndentTypeStart(Position),
    IndentTypeEnd(Position),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EGeneratesWith {
    Open(Position),
    With(Position),
    IndentWith(Position),
    IndentListStart(Position),
    IndentListEnd(Position),
    ListStart(Position),
    ListEnd(Position),
    Identifier(Position),
    Space(BadInputError, Position),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BadInputError {
    HasTab,
    ///
    TooManyLines,
    ///
    ///
    BadUtf8,
}

pub fn bad_input_to_syntax_error<'a>(bad_input: BadInputError) -> SyntaxError<'a> {
    use crate::parser::BadInputError::*;
    match bad_input {
        HasTab => SyntaxError::NotYetImplemented("call error on tabs".to_string()),
        TooManyLines => SyntaxError::TooManyLines,
        BadUtf8 => SyntaxError::BadUtf8,
    }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EExpr<'a> {
    Start(Position),
    End(Position),
    BadExprEnd(Position),
    Space(BadInputError, Position),

    Dot(Position),
    Access(Position),
    UnaryNot(Position),
    UnaryNegate(Position),
    BadOperator(&'a str, Position),

    DefMissingFinalExpr(Position),
    DefMissingFinalExpr2(&'a EExpr<'a>, Position),
    Type(EType<'a>, Position),
    Pattern(&'a EPattern<'a>, Position),
    IndentDefBody(Position),
    IndentEquals(Position),
    IndentAnnotation(Position),
    Equals(Position),
    Colon(Position),
    DoubleColon(Position),
    Ident(Position),
    ElmStyleFunction(Region, Position),
    MalformedPattern(Position),
    QualifiedTag(Position),
    BackpassComma(Position),
    BackpassArrow(Position),

    When(EWhen<'a>, Position),
    If(EIf<'a>, Position),

    Expect(EExpect<'a>, Position),

    Lambda(ELambda<'a>, Position),
    Underscore(Position),

    InParens(EInParens<'a>, Position),
    Record(ERecord<'a>, Position),
    Str(EString<'a>, Position),
    Number(ENumber, Position),
    List(EList<'a>, Position),

    IndentStart(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ENumber {
    End,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EString<'a> {
    Open(Position),

    CodePtOpen(Position),
    CodePtEnd(Position),

    Space(BadInputError, Position),
    EndlessSingle(Position),
    EndlessMulti(Position),
    UnknownEscape(Position),
    Format(&'a EExpr<'a>, Position),
    FormatEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ERecord<'a> {
    End(Position),
    Open(Position),

    Updateable(Position),
    Field(Position),
    Colon(Position),
    QuestionMark(Position),
    Bar(Position),
    Ampersand(Position),

    // TODO remove
    Expr(&'a EExpr<'a>, Position),

    Space(BadInputError, Position),

    IndentOpen(Position),
    IndentColon(Position),
    IndentBar(Position),
    IndentAmpersand(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EInParens<'a> {
    End(Position),
    Open(Position),
    ///
    Expr(&'a EExpr<'a>, Position),

    ///
    Space(BadInputError, Position),
    ///
    IndentOpen(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ELambda<'a> {
    Space(BadInputError, Position),
    Start(Position),
    Arrow(Position),
    Comma(Position),
    Arg(Position),
    // TODO make EEXpr
    Pattern(EPattern<'a>, Position),
    Body(&'a EExpr<'a>, Position),
    IndentArrow(Position),
    IndentBody(Position),
    IndentArg(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EList<'a> {
    Open(Position),
    End(Position),
    Space(BadInputError, Position),

    Expr(&'a EExpr<'a>, Position),

    IndentOpen(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EWhen<'a> {
    Space(BadInputError, Position),
    When(Position),
    Is(Position),
    Pattern(EPattern<'a>, Position),
    Arrow(Position),
    Bar(Position),

    IfToken(Position),
    IfGuard(&'a EExpr<'a>, Position),

    Condition(&'a EExpr<'a>, Position),
    Branch(&'a EExpr<'a>, Position),

    IndentIs(Position),
    IndentCondition(Position),
    IndentPattern(Position),
    IndentArrow(Position),
    IndentBranch(Position),
    IndentIfGuard(Position),
    PatternAlignment(u32, Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EIf<'a> {
    Space(BadInputError, Position),
    If(Position),
    Then(Position),
    Else(Position),
    // TODO make EEXpr
    Condition(&'a EExpr<'a>, Position),
    ThenBranch(&'a EExpr<'a>, Position),
    ElseBranch(&'a EExpr<'a>, Position),

    IndentCondition(Position),
    IndentIf(Position),
    IndentThenToken(Position),
    IndentElseToken(Position),
    IndentThenBranch(Position),
    IndentElseBranch(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EExpect<'a> {
    Space(BadInputError, Position),
    Expect(Position),
    Condition(&'a EExpr<'a>, Position),
    Continuation(&'a EExpr<'a>, Position),
    IndentCondition(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPattern<'a> {
    Record(PRecord<'a>, Position),
    Underscore(Position),

    Start(Position),
    End(Position),
    Space(BadInputError, Position),

    PInParens(PInParens<'a>, Position),
    NumLiteral(ENumber, Position),

    IndentStart(Position),
    IndentEnd(Position),
    AsIndentStart(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PRecord<'a> {
    End(Position),
    Open(Position),

    Field(Position),
    Colon(Position),
    Optional(Position),

    Pattern(&'a EPattern<'a>, Position),
    Expr(&'a EExpr<'a>, Position),

    Space(BadInputError, Position),

    IndentOpen(Position),
    IndentColon(Position),
    IndentOptional(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PInParens<'a> {
    End(Position),
    Open(Position),
    Pattern(&'a EPattern<'a>, Position),

    Space(BadInputError, Position),
    IndentOpen(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EType<'a> {
    Space(BadInputError, Position),

    TRecord(ETypeRecord<'a>, Position),
    TTagUnion(ETypeTagUnion<'a>, Position),
    TInParens(ETypeInParens<'a>, Position),
    TApply(ETypeApply, Position),
    TInlineAlias(ETypeInlineAlias, Position),
    TBadTypeVariable(Position),
    TWildcard(Position),
    TInferred(Position),
    ///
    TStart(Position),
    TEnd(Position),
    TFunctionArgument(Position),
    ///
    TIndentStart(Position),
    TIndentEnd(Position),
    TAsIndentStart(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeRecord<'a> {
    End(Position),
    Open(Position),

    Field(Position),
    Colon(Position),
    Optional(Position),
    Type(&'a EType<'a>, Position),

    Space(BadInputError, Position),

    IndentOpen(Position),
    IndentColon(Position),
    IndentOptional(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeTagUnion<'a> {
    End(Position),
    Open(Position),

    Type(&'a EType<'a>, Position),

    Space(BadInputError, Position),

    IndentOpen(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeInParens<'a> {
    End(Position),
    Open(Position),
    ///
    Type(&'a EType<'a>, Position),

    ///
    Space(BadInputError, Position),
    ///
    IndentOpen(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeApply {
    ///
    StartNotUppercase(Position),
    End(Position),
    Space(BadInputError, Position),
    ///
    DoubleDot(Position),
    TrailingDot(Position),
    StartIsNumber(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeInlineAlias {
    NotAnAlias(Position),
    Qualified(Position),
    ArgumentNotLowercase(Position),
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
    fn parse(&self, _: &'a Bump, _: State<'a>) -> ParseResult<'a, Output, Error>;

    #[cfg(not(feature = "parse_debug_trace"))]
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
    F: Fn(&'a Bump, State<'a>) -> ParseResult<'a, Output, Error>,
{
    fn parse(&self, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Output, Error> {
        self(arena, state)
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
    fn parse(&self, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, O, E> {
        use std::cell::RefCell;

        thread_local! {
            pub static INDENT: RefCell<usize> = RefCell::new(0);
        }

        // This should be enough for anyone. Right? RIGHT?
        let indent_text =
            "| ; : ! | ; : ! | ; : ! | ; : ! | ; : ! | ; : ! | ; : ! | ; : ! | ; : ! ";

        let cur_indent = INDENT.with(|i| *i.borrow());

        println!(
            "@{:>5}:{:<5}: {}{:<50}",
            state.line,
            state.column,
            &indent_text[..cur_indent * 2],
            self.message
        );

        INDENT.with(|i| *i.borrow_mut() += 1);
        let res = self.parser.parse(arena, state);
        INDENT.with(|i| *i.borrow_mut() = cur_indent);

        let (progress, value, state) = match &res {
            Ok((progress, result, state)) => (progress, Ok(result), state),
            Err((progress, error, state)) => (progress, Err(error), state),
        };

        println!(
            "@{:>5}:{:<5}: {}{:<50} {:<15} {:?}",
            state.line,
            state.column,
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
    ToError: Fn(Position) -> E,
    E: 'a,
{
    move |_, mut state: State<'a>| {
        let width = keyword.len();

        if !state.bytes().starts_with(keyword.as_bytes()) {
            return Err((NoProgress, if_error(state.pos()), state));
        }

        // the next character should not be an identifier character
        // to prevent treating `whence` or `iffy` as keywords
        match state.bytes().get(width) {
            Some(next) if *next == b' ' || *next == b'#' || *next == b'\n' => {
                state = state.advance(width);
                Ok((MadeProgress, (), state))
            }
            None => {
                state = state.advance(width);
                Ok((MadeProgress, (), state))
            }
            Some(_) => Err((NoProgress, if_error(state.pos()), state)),
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
        let start_bytes_len = state.bytes().len();

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
                                    let progress = Progress::from_lengths(
                                        start_bytes_len,
                                        state.bytes().len(),
                                    );

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
        let start_bytes_len = state.bytes().len();

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
                                        old_state.bytes().len(),
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
        let start_bytes_len = state.bytes().len();

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
                                        old_state.bytes().len(),
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
    V: Fn(Position) -> Error,
    Error: 'a,
{
    move |arena, state: State<'a>| {
        let start_bytes_len = state.bytes().len();

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
                                    return Err((NoProgress, to_element_error(state.pos()), state));
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
                                        old_state.bytes().len(),
                                    );
                                    return Ok((progress, buf, old_state));
                                }
                            }
                        }
                    }
                }
            }

            Err((MadeProgress, fail, state)) => Err((MadeProgress, fail, state)),
            Err((NoProgress, _fail, state)) => {
                Err((NoProgress, to_element_error(state.pos()), state))
            }
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
        move |arena, state: $crate::state::State<'a>| {
            use roc_region::all::{Loc, Region};

            let start = state.pos();

            match $parser.parse(arena, state) {
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
        move |arena, state: $crate::state::State<'a>| {
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
        move |arena, state: $crate::state::State<'a>| {
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
    ($opening_brace:expr, $elem:expr, $delimiter:expr, $closing_brace:expr, $min_indent:expr, $open_problem:expr, $indent_problem:expr, $space_before:expr) => {
        skip_first!(
            $opening_brace,
            |arena, state| {
                let (_, spaces, state) = space0_e($min_indent, $indent_problem)
                    .parse(arena, state)?;

                let (_, (mut parsed_elems, mut final_comments), state) =
                                and!(
                                    $crate::parser::trailing_sep_by0(
                                        $delimiter,
                                        $crate::blankspace::space0_before_optional_after(
                                            $elem,
                                            $min_indent,
                                            $indent_problem,
                                            $indent_problem
                                        )
                                    ),
                                    $crate::blankspace::space0_e($min_indent, $indent_problem)
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
        move |_arena: &'a bumpalo::Bump, state: $crate::state::State<'a>| {
            Ok((NoProgress, $value, state))
        }
    };
}

#[macro_export]
macro_rules! and {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>| {
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
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>| {

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
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>| match $p1
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
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>| {

            match $p1.parse(arena, state) {
                valid @ Ok(_) => valid,
                Err((MadeProgress, fail, state)) => Err((MadeProgress, fail, state )),
                Err((NoProgress, _, state)) => Err((MadeProgress, $toerror(state.pos()), state)),
            }
        }
    };

    ($toerror:expr; $p1:expr, $($others:expr),+) => {
        one_of_with_error!($toerror, $p1, one_of_with_error!($($others),+))
    };
}

pub fn specialize<'a, F, P, T, X, Y>(map_error: F, parser: P) -> impl Parser<'a, T, Y>
where
    F: Fn(X, Position) -> Y,
    P: Parser<'a, T, X>,
    Y: 'a,
{
    move |a, s| match parser.parse(a, s) {
        Ok(t) => Ok(t),
        Err((p, error, s)) => Err((p, map_error(error, s.pos()), s)),
    }
}

/// Like `specialize`, except the error function receives a Region representing the begin/end of the error
pub fn specialize_region<'a, F, P, T, X, Y>(map_error: F, parser: P) -> impl Parser<'a, T, Y>
where
    F: Fn(X, Region) -> Y,
    P: Parser<'a, T, X>,
    Y: 'a,
{
    move |a, s: State<'a>| {
        let start = s.pos();
        match parser.parse(a, s) {
            Ok(t) => Ok(t),
            Err((p, error, s)) => Err((p, map_error(error, Region::new(start, s.pos())), s)),
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
    move |a, s| match parser.parse(a, s) {
        Ok(t) => Ok(t),
        Err((p, error, s)) => Err((p, map_error(a.alloc(error), s.pos()), s)),
    }
}

pub fn word1<'a, ToError, E>(word: u8, to_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    debug_assert_ne!(word, b'\n');

    move |_arena: &'a Bump, state: State<'a>| match state.bytes().get(0) {
        Some(x) if *x == word => {
            let state = state.advance(1);
            Ok((MadeProgress, (), state))
        }
        _ => Err((NoProgress, to_error(state.pos()), state)),
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

    move |_arena: &'a Bump, state: State<'a>| {
        if state.bytes().starts_with(&needle) {
            let state = state.advance(2);
            Ok((MadeProgress, (), state))
        } else {
            Err((NoProgress, to_error(state.pos()), state))
        }
    }
}

pub fn check_indent<'a, TE, E>(min_indent: u32, to_problem: TE) -> impl Parser<'a, (), E>
where
    TE: Fn(Position) -> E,
    E: 'a,
{
    move |_arena, state: State<'a>| {
        dbg!(state.indent_column, min_indent);
        if state.indent_column < min_indent {
            Err((NoProgress, to_problem(state.pos()), state))
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

            let start_bytes_len = state.bytes().len();

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
                                        let progress = Progress::from_lengths(start_bytes_len, old_state.bytes().len());
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
                Err((progress, _, new_state)) => {
                    Err((progress, $to_error(new_state.pos), new_state))
                }
            }
        }
    };
}

#[macro_export]
macro_rules! debug {
    ($parser:expr) => {
        move |arena, state: $crate::state::State<'a>| dbg!($parser.parse(arena, state))
    };
}

#[macro_export]
macro_rules! either {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>| match $p1
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
    move |arena: &'a Bump, state: State<'a>| {
        let old_state = state.clone();

        match parser.parse(arena, state) {
            Ok((_, a, s1)) => Ok((NoProgress, a, s1)),
            Err((_, f, _)) => Err((NoProgress, f, old_state)),
        }
    }
}
