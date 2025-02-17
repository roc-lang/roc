use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
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
impl<'a> SyntaxError<'a> {
    pub fn get_region(&self) -> Option<Region> {
        match self {
            SyntaxError::Unexpected(r) => Some(*r),
            SyntaxError::Eof(r) => Some(*r),
            SyntaxError::ReservedKeyword(r) => Some(*r),
            SyntaxError::ArgumentsBeforeEquals(r) => Some(*r),
            SyntaxError::Type(e_type) => Some(e_type.get_region()),
            SyntaxError::Pattern(e_pattern) => Some(e_pattern.get_region()),
            SyntaxError::NotEndOfFile(pos) => Some(Region::from_pos(*pos)),
            SyntaxError::Expr(e_expr, _) => Some(e_expr.get_region()),
            SyntaxError::Header(e_header) => Some(e_header.get_region()),
            SyntaxError::NotYetImplemented(_) => None,
            SyntaxError::OutdentedTooFar => None,
            SyntaxError::Todo => None,
            SyntaxError::InvalidPattern => None,
            SyntaxError::BadUtf8 => None,
            SyntaxError::Space(_bad_input) => None,
        }
    }
}
pub trait SpaceProblem: std::fmt::Debug {
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
    EHeader<'a>,
    EIf<'a>,
    EImport<'a>,
    EParams<'a>,
    EImports,
    EImportParams<'a>,
    EInParens<'a>,
    EClosure<'a>,
    EList<'a>,
    EPackageEntry<'a>,
    EPackages<'a>,
    EPattern<'a>,
    EProvides<'a>,
    ERecord<'a>,
    EReturn<'a>,
    ERequires<'a>,
    EString<'a>,
    EType<'a>,
    ETypeInParens<'a>,
    ETypeRecord<'a>,
    ETypeTagUnion<'a>,
    ETypedIdent<'a>,
    ETypeAbilityImpl<'a>,
    EWhen<'a>,
    EAbility<'a>,
    PInParens<'a>,
    PRecord<'a>,
    PList<'a>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EHeader<'a> {
    Provides(EProvides<'a>, Position),
    Params(EParams<'a>, Position),
    Exposes(EExposes, Position),
    Imports(EImports, Position),
    Requires(ERequires<'a>, Position),
    Packages(EPackages<'a>, Position),

    Space(BadInputError, Position),
    Start(Position),
    ModuleName(Position),
    AppName(EString<'a>, Position),
    PackageName(EPackageName<'a>, Position),
    PlatformName(EPackageName<'a>, Position),
    IndentStart(Position),

    InconsistentModuleName(Region),
}

impl<'a> EHeader<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            EHeader::Provides(provides, _pos) => provides.get_region(),
            EHeader::Params(params, _pos) => params.get_region(),
            EHeader::Exposes(_, pos) => Region::from_pos(*pos),
            EHeader::Imports(_, pos) => Region::from_pos(*pos),
            EHeader::Requires(requires, _pos) => requires.get_region(),
            EHeader::Packages(packages, _pos) => packages.get_region(),
            EHeader::Space(_, pos) => Region::from_pos(*pos),
            EHeader::Start(pos) => Region::from_pos(*pos),
            EHeader::ModuleName(pos) => Region::from_pos(*pos),
            EHeader::AppName(app_name, _pos) => app_name.get_region(),
            EHeader::PackageName(package_name, _pos) => package_name.get_region(),
            EHeader::PlatformName(platform_name, _pos) => platform_name.get_region(),
            EHeader::IndentStart(pos) => Region::from_pos(*pos),
            EHeader::InconsistentModuleName(region) => *region,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EProvides<'a> {
    Provides(Position),
    Open(Position),
    To(Position),
    IndentProvides(Position),
    IndentTo(Position),
    IndentListStart(Position),
    IndentPackage(Position),
    ListStart(Position),
    ListEnd(Position),
    Identifier(Position),
    Package(EPackageName<'a>, Position),
    Space(BadInputError, Position),
}

impl<'a> EProvides<'a> {
    pub fn get_region(&self) -> Region {
        let pos = match self {
            EProvides::Provides(p)
            | EProvides::Open(p)
            | EProvides::To(p)
            | EProvides::IndentProvides(p)
            | EProvides::IndentTo(p)
            | EProvides::IndentListStart(p)
            | EProvides::IndentPackage(p)
            | EProvides::ListStart(p)
            | EProvides::ListEnd(p)
            | EProvides::Identifier(p)
            | EProvides::Package(_, p)
            | EProvides::Space(_, p) => p,
        };
        Region::from_pos(*pos)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EParams<'a> {
    Pattern(PRecord<'a>, Position),
    BeforeArrow(Position),
    Arrow(Position),
    AfterArrow(Position),
    Space(BadInputError, Position),
}

impl<'a> EParams<'a> {
    pub fn get_region(&self) -> Region {
        let pos = match self {
            EParams::Pattern(_, p)
            | EParams::BeforeArrow(p)
            | EParams::Arrow(p)
            | EParams::AfterArrow(p)
            | EParams::Space(_, p) => p,
        };
        Region::from_pos(*pos)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EExposes {
    Exposes(Position),
    Open(Position),
    IndentExposes(Position),
    IndentListStart(Position),
    ListStart(Position),
    ListEnd(Position),
    Identifier(Position),
    Space(BadInputError, Position),
}

impl EExposes {
    pub fn get_region(&self) -> Region {
        let pos = match self {
            EExposes::Exposes(p)
            | EExposes::Open(p)
            | EExposes::IndentExposes(p)
            | EExposes::IndentListStart(p)
            | EExposes::ListStart(p)
            | EExposes::ListEnd(p)
            | EExposes::Identifier(p)
            | EExposes::Space(_, p) => p,
        };
        Region::from_pos(*pos)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ERequires<'a> {
    Requires(Position),
    Open(Position),
    IndentRequires(Position),
    IndentListStart(Position),
    ListStart(Position),
    ListEnd(Position),
    TypedIdent(ETypedIdent<'a>, Position),
    Rigid(Position),
    Space(BadInputError, Position),
}

impl<'a> ERequires<'a> {
    pub fn get_region(&self) -> Region {
        let pos = match self {
            ERequires::Requires(p)
            | ERequires::Open(p)
            | ERequires::IndentRequires(p)
            | ERequires::IndentListStart(p)
            | ERequires::ListStart(p)
            | ERequires::ListEnd(p)
            | ERequires::TypedIdent(_, p)
            | ERequires::Rigid(p)
            | ERequires::Space(_, p) => p,
        };
        Region::from_pos(*pos)
    }
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

impl<'a> ETypedIdent<'a> {
    pub fn get_region(&self) -> Region {
        let pos = match self {
            ETypedIdent::Space(_, p)
            | ETypedIdent::HasType(p)
            | ETypedIdent::IndentHasType(p)
            | ETypedIdent::Name(p)
            | ETypedIdent::Type(_, p)
            | ETypedIdent::IndentType(p)
            | ETypedIdent::Identifier(p) => p,
        };
        Region::from_pos(*pos)
    }
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

impl<'a> EPackages<'a> {
    pub fn get_region(&self) -> Region {
        let pos = match self {
            EPackages::Open(p)
            | EPackages::Space(_, p)
            | EPackages::Packages(p)
            | EPackages::IndentPackages(p)
            | EPackages::ListStart(p)
            | EPackages::ListEnd(p)
            | EPackages::IndentListStart(p)
            | EPackages::IndentListEnd(p)
            | EPackages::PackageEntry(_, p) => p,
        };
        Region::from_pos(*pos)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPackageName<'a> {
    BadPath(EString<'a>, Position),
    Escapes(Position),
    Multiline(Position),
}

impl<'a> EPackageName<'a> {
    pub fn get_region(&self) -> Region {
        let pos = match self {
            EPackageName::BadPath(_, p) | EPackageName::Escapes(p) | EPackageName::Multiline(p) => {
                p
            }
        };
        Region::from_pos(*pos)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPackageEntry<'a> {
    BadPackage(EPackageName<'a>, Position),
    Shorthand(Position),
    Colon(Position),
    IndentPackage(Position),
    IndentPlatform(Position),
    Platform(Position),
    Space(BadInputError, Position),
}

impl<'a> EPackageEntry<'a> {
    pub fn get_region(&self) -> Region {
        let pos = match self {
            EPackageEntry::BadPackage(_, p)
            | EPackageEntry::Shorthand(p)
            | EPackageEntry::Colon(p)
            | EPackageEntry::IndentPackage(p)
            | EPackageEntry::IndentPlatform(p)
            | EPackageEntry::Platform(p)
            | EPackageEntry::Space(_, p) => p,
        };
        Region::from_pos(*pos)
    }
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
    SetStart(Position),
    SetEnd(Position),
    TypedIdent(Position),
    AsKeyword(Position),
    StrLiteral(Position),
}

impl EImports {
    pub fn get_region(&self) -> Region {
        let pos = match self {
            EImports::Open(p)
            | EImports::Imports(p)
            | EImports::IndentImports(p)
            | EImports::IndentListStart(p)
            | EImports::IndentListEnd(p)
            | EImports::ListStart(p)
            | EImports::ListEnd(p)
            | EImports::Identifier(p)
            | EImports::ExposingDot(p)
            | EImports::ShorthandDot(p)
            | EImports::Shorthand(p)
            | EImports::ModuleName(p)
            | EImports::Space(_, p)
            | EImports::IndentSetStart(p)
            | EImports::SetStart(p)
            | EImports::SetEnd(p)
            | EImports::TypedIdent(p)
            | EImports::AsKeyword(p)
            | EImports::StrLiteral(p) => p,
        };
        Region::from_pos(*pos)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BadInputError {
    HasTab,
    HasMisplacedCarriageReturn,
    HasAsciiControl,
    BadUtf8,
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
    TrailingOperator(Position),

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
    Ability(EAbility<'a>, Position),
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
    DbgContinue(Position),

    When(EWhen<'a>, Position),
    If(EIf<'a>, Position),

    Expect(EExpect<'a>, Position),
    Dbg(EExpect<'a>, Position),
    Import(EImport<'a>, Position),
    Return(EReturn<'a>, Position),

    Closure(EClosure<'a>, Position),
    Underscore(Position),
    Crash(Position),
    Try(Position),

    InParens(EInParens<'a>, Position),
    Record(ERecord<'a>, Position),
    RecordUpdateOldBuilderField(Region),
    RecordUpdateIgnoredField(Region),
    RecordBuilderOldBuilderField(Region),

    // SingleQuote errors are folded into the EString
    Str(EString<'a>, Position),

    Number(ENumber, Position),
    List(EList<'a>, Position),

    IndentStart(Position),
    IndentEnd(Position),

    UnexpectedComma(Position),
    UnexpectedTopLevelExpr(Position),
}

impl<'a> EExpr<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Cases with child nodes that have get_region()
            EExpr::Type(e_type, _) => e_type.get_region(),
            EExpr::Pattern(e_pattern, _) => e_pattern.get_region(),
            EExpr::Ability(e_ability, _) => e_ability.get_region(),
            EExpr::When(e_when, _) => e_when.get_region(),
            EExpr::If(e_if, _) => e_if.get_region(),
            EExpr::Expect(e_expect, _) => e_expect.get_region(),
            EExpr::Dbg(e_expect, _) => e_expect.get_region(),
            EExpr::Import(e_import, _) => e_import.get_region(),
            EExpr::Return(e_return, _) => e_return.get_region(),
            EExpr::Closure(e_closure, _) => e_closure.get_region(),
            EExpr::InParens(e_in_parens, _) => e_in_parens.get_region(),
            EExpr::Record(e_record, _) => e_record.get_region(),
            EExpr::Str(e_string, _) => e_string.get_region(),
            EExpr::List(e_list, _) => e_list.get_region(),

            // Cases with direct Region values
            EExpr::RecordUpdateOldBuilderField(r)
            | EExpr::RecordUpdateIgnoredField(r)
            | EExpr::RecordBuilderOldBuilderField(r) => *r,

            // Cases with Position values
            EExpr::TrailingOperator(p)
            | EExpr::Start(p)
            | EExpr::End(p)
            | EExpr::BadExprEnd(p)
            | EExpr::Space(_, p)
            | EExpr::Dot(p)
            | EExpr::Access(p)
            | EExpr::UnaryNot(p)
            | EExpr::UnaryNegate(p)
            | EExpr::BadOperator(_, p)
            | EExpr::DefMissingFinalExpr(p)
            | EExpr::DefMissingFinalExpr2(_, p)
            | EExpr::IndentDefBody(p)
            | EExpr::IndentEquals(p)
            | EExpr::IndentAnnotation(p)
            | EExpr::Equals(p)
            | EExpr::Colon(p)
            | EExpr::DoubleColon(p)
            | EExpr::Ident(p)
            | EExpr::ElmStyleFunction(_, p)
            | EExpr::MalformedPattern(p)
            | EExpr::QualifiedTag(p)
            | EExpr::DbgContinue(p)
            | EExpr::Underscore(p)
            | EExpr::Crash(p)
            | EExpr::Try(p)
            | EExpr::Number(_, p)
            | EExpr::IndentStart(p)
            | EExpr::IndentEnd(p)
            | EExpr::UnexpectedComma(p)
            | EExpr::UnexpectedTopLevelExpr(p) => Region::from_pos(*p),
        }
    }
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

    InvalidSingleQuote(ESingleQuote, Position),

    Space(BadInputError, Position),
    EndlessSingleLine(Position),
    EndlessMultiLine(Position),
    EndlessSingleQuote(Position),
    UnknownEscape(Position),
    Format(&'a EExpr<'a>, Position),
    FormatEnd(Position),
    MultilineInsufficientIndent(Position),
    ExpectedDoubleQuoteGotSingleQuote(Position),
    InvalidUnicodeCodepoint(Region),
    UnicodeEscapeTooLarge(Region),
}

impl<'a> EString<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Case with child node that has get_region()
            EString::Format(expr, _) => expr.get_region(),

            // Cases with Position values
            EString::Open(p)
            | EString::CodePtOpen(p)
            | EString::CodePtEnd(p)
            | EString::InvalidSingleQuote(_, p)
            | EString::Space(_, p)
            | EString::EndlessSingleLine(p)
            | EString::EndlessMultiLine(p)
            | EString::EndlessSingleQuote(p)
            | EString::UnknownEscape(p)
            | EString::FormatEnd(p)
            | EString::MultilineInsufficientIndent(p)
            | EString::ExpectedDoubleQuoteGotSingleQuote(p) => Region::from_pos(*p),
            EString::InvalidUnicodeCodepoint(region) | EString::UnicodeEscapeTooLarge(region) => {
                *region
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ESingleQuote {
    Empty,
    TooLong,
    InterpolationNotAllowed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ERecord<'a> {
    End(Position),
    Open(Position),

    Prefix(Position),
    Field(Position),
    UnderscoreField(Position),
    Colon(Position),
    QuestionMark(Position),
    SecondQuestionMark(Position),
    Arrow(Position),
    Ampersand(Position),

    // TODO remove
    Expr(&'a EExpr<'a>, Position),

    Space(BadInputError, Position),
}

impl<'a> ERecord<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Cases with child node that has get_region()
            ERecord::Expr(expr, _) => expr.get_region(),

            // Cases with Position values
            ERecord::End(p)
            | ERecord::Open(p)
            | ERecord::Prefix(p)
            | ERecord::Field(p)
            | ERecord::UnderscoreField(p)
            | ERecord::Colon(p)
            | ERecord::QuestionMark(p)
            | ERecord::SecondQuestionMark(p)
            | ERecord::Arrow(p)
            | ERecord::Ampersand(p)
            | ERecord::Space(_, p) => Region::from_pos(*p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EInParens<'a> {
    End(Position),
    Open(Position),

    /// Empty parens, e.g. () is not allowed
    Empty(Position),

    ///
    Expr(&'a EExpr<'a>, Position),

    ///
    Space(BadInputError, Position),
}

impl<'a> EInParens<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Cases with child node that has get_region()
            EInParens::Expr(expr, _) => expr.get_region(),

            // Cases with Position values
            EInParens::End(p)
            | EInParens::Open(p)
            | EInParens::Empty(p)
            | EInParens::Space(_, p) => Region::from_pos(*p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EClosure<'a> {
    Bar(Position),
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

impl<'a> EClosure<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Cases with child nodes that have get_region()
            EClosure::Pattern(pattern, _) => pattern.get_region(),
            EClosure::Body(expr, _) => expr.get_region(),

            // Cases with Position values
            EClosure::Bar(p)
            | EClosure::Space(_, p)
            | EClosure::Start(p)
            | EClosure::Arrow(p)
            | EClosure::Comma(p)
            | EClosure::Arg(p)
            | EClosure::IndentArrow(p)
            | EClosure::IndentBody(p)
            | EClosure::IndentArg(p) => Region::from_pos(*p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EList<'a> {
    Open(Position),
    End(Position),
    Space(BadInputError, Position),

    Expr(&'a EExpr<'a>, Position),
}

impl<'a> EList<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Case with child node that has get_region()
            EList::Expr(expr, _) => expr.get_region(),

            // Cases with Position values
            EList::Open(p) | EList::End(p) | EList::Space(_, p) => Region::from_pos(*p),
        }
    }
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

    IndentCondition(Position),
    IndentPattern(Position),
    IndentArrow(Position),
    IndentBranch(Position),
    IndentIfGuard(Position),
    PatternAlignment(u32, Position),
}

impl<'a> EWhen<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Cases with child nodes that have get_region()
            EWhen::Pattern(pattern, _) => pattern.get_region(),
            EWhen::IfGuard(expr, _) => expr.get_region(),
            EWhen::Condition(expr, _) => expr.get_region(),
            EWhen::Branch(expr, _) => expr.get_region(),

            // Cases with Position values
            EWhen::Space(_, p)
            | EWhen::When(p)
            | EWhen::Is(p)
            | EWhen::Arrow(p)
            | EWhen::Bar(p)
            | EWhen::IfToken(p)
            | EWhen::IndentCondition(p)
            | EWhen::IndentPattern(p)
            | EWhen::IndentArrow(p)
            | EWhen::IndentBranch(p)
            | EWhen::IndentIfGuard(p)
            | EWhen::PatternAlignment(_, p) => Region::from_pos(*p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EAbility<'a> {
    Space(BadInputError, Position),
    Type(EType<'a>, Position),

    DemandAlignment(i32, Position),
    DemandName(Position),
    DemandColon(Position),
}

impl<'a> EAbility<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Case with child node that has get_region()
            EAbility::Type(e_type, _) => e_type.get_region(),

            // Cases with Position values
            EAbility::Space(_, p)
            | EAbility::DemandAlignment(_, p)
            | EAbility::DemandName(p)
            | EAbility::DemandColon(p) => Region::from_pos(*p),
        }
    }
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

impl<'a> EIf<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            EIf::Condition(expr, _) | EIf::ThenBranch(expr, _) | EIf::ElseBranch(expr, _) => {
                expr.get_region()
            }
            EIf::Space(_, p)
            | EIf::If(p)
            | EIf::Then(p)
            | EIf::Else(p)
            | EIf::IndentCondition(p)
            | EIf::IndentIf(p)
            | EIf::IndentThenToken(p)
            | EIf::IndentElseToken(p)
            | EIf::IndentThenBranch(p)
            | EIf::IndentElseBranch(p) => Region::from_pos(*p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EExpect<'a> {
    Space(BadInputError, Position),
    Dbg(Position),
    Expect(Position),
    Condition(&'a EExpr<'a>, Position),
    Continuation(&'a EExpr<'a>, Position),
    IndentCondition(Position),
}

impl<'a> EExpect<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            EExpect::Condition(expr, _) | EExpect::Continuation(expr, _) => expr.get_region(),
            EExpect::Space(_, p)
            | EExpect::Dbg(p)
            | EExpect::Expect(p)
            | EExpect::IndentCondition(p) => Region::from_pos(*p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EReturn<'a> {
    Space(BadInputError, Position),
    Return(Position),
    ReturnValue(&'a EExpr<'a>, Position),
    IndentReturnValue(Position),
}
impl<'a> EReturn<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            EReturn::ReturnValue(expr, _) => expr.get_region(),
            EReturn::Space(_, p) | EReturn::Return(p) | EReturn::IndentReturnValue(p) => {
                Region::from_pos(*p)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EImport<'a> {
    Import(Position),
    IndentStart(Position),
    PackageShorthand(Position),
    PackageShorthandDot(Position),
    ModuleName(Position),
    Params(EImportParams<'a>, Position),
    IndentAs(Position),
    As(Position),
    IndentAlias(Position),
    Alias(Position),
    LowercaseAlias(Region),
    IndentExposing(Position),
    Exposing(Position),
    ExposingListStart(Position),
    ExposedName(Position),
    ExposingListEnd(Position),
    IndentIngestedPath(Position),
    IngestedPath(Position),
    IndentIngestedName(Position),
    IngestedName(Position),
    IndentColon(Position),
    Colon(Position),
    IndentAnnotation(Position),
    Annotation(EType<'a>, Position),
    Space(BadInputError, Position),
    EndNewline(Position),
}

impl<'a> EImport<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Cases with child nodes that have get_region()
            EImport::Params(params, _) => params.get_region(),
            EImport::Annotation(e_type, _) => e_type.get_region(),

            // Case with direct Region value
            EImport::LowercaseAlias(r) => *r,

            // Cases with Position values
            EImport::Import(p)
            | EImport::IndentStart(p)
            | EImport::PackageShorthand(p)
            | EImport::PackageShorthandDot(p)
            | EImport::ModuleName(p)
            | EImport::IndentAs(p)
            | EImport::As(p)
            | EImport::IndentAlias(p)
            | EImport::Alias(p)
            | EImport::IndentExposing(p)
            | EImport::Exposing(p)
            | EImport::ExposingListStart(p)
            | EImport::ExposedName(p)
            | EImport::ExposingListEnd(p)
            | EImport::IndentIngestedPath(p)
            | EImport::IngestedPath(p)
            | EImport::IndentIngestedName(p)
            | EImport::IngestedName(p)
            | EImport::IndentColon(p)
            | EImport::Colon(p)
            | EImport::IndentAnnotation(p)
            | EImport::Space(_, p)
            | EImport::EndNewline(p) => Region::from_pos(*p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EImportParams<'a> {
    Indent(Position),
    Record(ERecord<'a>, Position),
    RecordUpdateFound(Region),
    RecordBuilderFound(Region),
    RecordIgnoredFieldFound(Region),
    Space(BadInputError, Position),
}

impl<'a> EImportParams<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            EImportParams::Indent(p) | EImportParams::Record(_, p) | EImportParams::Space(_, p) => {
                Region::from_pos(*p)
            }
            EImportParams::RecordUpdateFound(r)
            | EImportParams::RecordBuilderFound(r)
            | EImportParams::RecordIgnoredFieldFound(r) => *r,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPattern<'a> {
    Record(PRecord<'a>, Position),
    List(PList<'a>, Position),
    AsKeyword(Position),
    AsIdentifier(Position),
    Underscore(Position),
    NotAPattern(Position),

    Start(Position),
    End(Position),
    Space(BadInputError, Position),

    PInParens(PInParens<'a>, Position),
    NumLiteral(ENumber, Position),

    IndentStart(Position),
    IndentEnd(Position),
    AsIndentStart(Position),

    AccessorFunction(Position),
    RecordUpdaterFunction(Position),
    Str(EString<'a>, Position),

    ParenStart(Position),
    ParenEnd(Position),
}

impl<'a> EPattern<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Cases with child nodes that have get_region()
            EPattern::Record(expr, _) => expr.get_region(),
            EPattern::List(expr, _) => expr.get_region(),
            EPattern::PInParens(expr, _) => expr.get_region(),
            EPattern::Str(e_string, _) => e_string.get_region(),

            // Cases with Position values
            EPattern::AsKeyword(position)
            | EPattern::AsIdentifier(position)
            | EPattern::Underscore(position)
            | EPattern::NotAPattern(position)
            | EPattern::Start(position)
            | EPattern::End(position)
            | EPattern::Space(_, position)
            | EPattern::NumLiteral(_, position)
            | EPattern::IndentStart(position)
            | EPattern::IndentEnd(position)
            | EPattern::AsIndentStart(position)
            | EPattern::AccessorFunction(position)
            | EPattern::RecordUpdaterFunction(position)
            | EPattern::ParenStart(position)
            | EPattern::ParenEnd(position) => Region::from_pos(*position),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PRecord<'a> {
    End(Position),
    Open(Position),

    Field(Position),
    Colon(Position),
    OptionalFirst(Position),
    OptionalSecond(Position),

    Pattern(&'a EPattern<'a>, Position),
    Expr(&'a EExpr<'a>, Position),

    Space(BadInputError, Position),
}

impl<'a> PRecord<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Cases with child nodes that have get_region()
            PRecord::Pattern(pattern, _) => pattern.get_region(),
            PRecord::Expr(expr, _) => expr.get_region(),

            // Cases with Position values
            PRecord::End(p)
            | PRecord::Open(p)
            | PRecord::Field(p)
            | PRecord::Colon(p)
            | PRecord::OptionalFirst(p)
            | PRecord::OptionalSecond(p)
            | PRecord::Space(_, p) => Region::from_pos(*p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PList<'a> {
    End(Position),
    Open(Position),

    Rest(Position),
    Pattern(&'a EPattern<'a>, Position),

    Space(BadInputError, Position),
}

impl<'a> PList<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Case with child node that has get_region()
            PList::Pattern(pattern, _) => pattern.get_region(),

            // Cases with Position values
            PList::End(p) | PList::Open(p) | PList::Rest(p) | PList::Space(_, p) => {
                Region::from_pos(*p)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PInParens<'a> {
    Empty(Position),
    End(Position),
    Open(Position),
    Pattern(&'a EPattern<'a>, Position),

    Space(BadInputError, Position),
}

impl<'a> PInParens<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Case with child node that has get_region()
            PInParens::Pattern(pattern, _) => pattern.get_region(),

            // Cases with Position values
            PInParens::Empty(p)
            | PInParens::End(p)
            | PInParens::Open(p)
            | PInParens::Space(_, p) => Region::from_pos(*p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EType<'a> {
    Space(BadInputError, Position),

    UnderscoreSpacing(Position),
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
    TWhereBar(Position),
    TImplementsClause(Position),
    TAbilityImpl(ETypeAbilityImpl<'a>, Position),
    ///
    TIndentStart(Position),
    TIndentEnd(Position),
    TAsIndentStart(Position),
}
impl<'a> EType<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Cases with child nodes that have get_region()
            EType::TRecord(expr, _) => expr.get_region(),
            EType::TTagUnion(expr, _) => expr.get_region(),
            EType::TInParens(expr, _) => expr.get_region(),
            EType::TApply(eapply, _) => eapply.get_region(),
            EType::TInlineAlias(einline, _) => einline.get_region(),
            EType::TAbilityImpl(eability, _) => eability.get_region(),

            // Cases with Position values
            EType::Space(_, p)
            | EType::UnderscoreSpacing(p)
            | EType::TBadTypeVariable(p)
            | EType::TWildcard(p)
            | EType::TInferred(p)
            | EType::TStart(p)
            | EType::TEnd(p)
            | EType::TFunctionArgument(p)
            | EType::TWhereBar(p)
            | EType::TImplementsClause(p)
            | EType::TIndentStart(p)
            | EType::TIndentEnd(p)
            | EType::TAsIndentStart(p) => Region::from_pos(*p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeRecord<'a> {
    End(Position),
    Open(Position),

    Field(Position),
    Colon(Position),
    OptionalFirst(Position),
    OptionalSecond(Position),
    Type(&'a EType<'a>, Position),

    Space(BadInputError, Position),

    IndentOpen(Position),
    IndentColon(Position),
    IndentOptional(Position),
    IndentEnd(Position),
}

impl<'a> ETypeRecord<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Case with child node that has get_region()
            ETypeRecord::Type(type_expr, _) => type_expr.get_region(),

            // Cases with Position values
            ETypeRecord::End(p)
            | ETypeRecord::Open(p)
            | ETypeRecord::Field(p)
            | ETypeRecord::Colon(p)
            | ETypeRecord::OptionalFirst(p)
            | ETypeRecord::OptionalSecond(p)
            | ETypeRecord::Space(_, p)
            | ETypeRecord::IndentOpen(p)
            | ETypeRecord::IndentColon(p)
            | ETypeRecord::IndentOptional(p)
            | ETypeRecord::IndentEnd(p) => Region::from_pos(*p),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeTagUnion<'a> {
    End(Position),
    Open(Position),

    Type(&'a EType<'a>, Position),

    Space(BadInputError, Position),
}

impl<'a> ETypeTagUnion<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Case with child node that has get_region()
            ETypeTagUnion::Type(type_expr, _) => type_expr.get_region(),

            // Cases with Position values
            ETypeTagUnion::End(p) | ETypeTagUnion::Open(p) | ETypeTagUnion::Space(_, p) => {
                Region::from_pos(*p)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeInParens<'a> {
    /// e.g. (), which isn't a valid type
    Empty(Position),

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

impl<'a> ETypeInParens<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Cases with child nodes that have get_region()
            ETypeInParens::Type(type_expr, _) => type_expr.get_region(),

            // Cases with Position values
            ETypeInParens::Empty(p)
            | ETypeInParens::End(p)
            | ETypeInParens::Open(p)
            | ETypeInParens::Space(_, p)
            | ETypeInParens::IndentOpen(p)
            | ETypeInParens::IndentEnd(p) => Region::from_pos(*p),
        }
    }
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

impl ETypeApply {
    pub fn get_region(&self) -> Region {
        let pos = match self {
            ETypeApply::StartNotUppercase(p)
            | ETypeApply::End(p)
            | ETypeApply::Space(_, p)
            | ETypeApply::DoubleDot(p)
            | ETypeApply::TrailingDot(p)
            | ETypeApply::StartIsNumber(p) => p,
        };
        Region::from_pos(*pos)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeInlineAlias {
    NotAnAlias(Position),
    Qualified(Position),
    ArgumentNotLowercase(Position),
}

impl ETypeInlineAlias {
    pub fn get_region(&self) -> Region {
        let pos = match self {
            ETypeInlineAlias::NotAnAlias(p)
            | ETypeInlineAlias::Qualified(p)
            | ETypeInlineAlias::ArgumentNotLowercase(p) => p,
        };
        Region::from_pos(*pos)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeAbilityImpl<'a> {
    End(Position),
    Open(Position),

    Field(Position),
    UnderscoreField(Position),
    Colon(Position),
    Arrow(Position),
    Optional(Position),
    Type(&'a EType<'a>, Position),

    Space(BadInputError, Position),

    Prefix(Position),
    QuestionMark(Position),
    SecondQuestionMark(Position),
    Ampersand(Position),
    Expr(&'a EExpr<'a>, Position),
    IndentBar(Position),
    IndentAmpersand(Position),
}
impl<'a> ETypeAbilityImpl<'a> {
    pub fn get_region(&self) -> Region {
        match self {
            // Case with child node that has get_region()
            ETypeAbilityImpl::Type(type_expr, _) => type_expr.get_region(),
            ETypeAbilityImpl::Expr(expr, _) => expr.get_region(),
            // Cases with Position values
            ETypeAbilityImpl::End(p)
            | ETypeAbilityImpl::Open(p)
            | ETypeAbilityImpl::Field(p)
            | ETypeAbilityImpl::UnderscoreField(p)
            | ETypeAbilityImpl::Colon(p)
            | ETypeAbilityImpl::Arrow(p)
            | ETypeAbilityImpl::Optional(p)
            | ETypeAbilityImpl::Space(_, p)
            | ETypeAbilityImpl::Prefix(p)
            | ETypeAbilityImpl::QuestionMark(p)
            | ETypeAbilityImpl::SecondQuestionMark(p)
            | ETypeAbilityImpl::Ampersand(p)
            | ETypeAbilityImpl::IndentBar(p)
            | ETypeAbilityImpl::IndentAmpersand(p) => Region::from_pos(*p),
        }
    }
}

impl<'a> From<ERecord<'a>> for ETypeAbilityImpl<'a> {
    fn from(e: ERecord<'a>) -> Self {
        match e {
            ERecord::End(p) => ETypeAbilityImpl::End(p),
            ERecord::Open(p) => ETypeAbilityImpl::Open(p),
            ERecord::Field(p) => ETypeAbilityImpl::Field(p),
            ERecord::UnderscoreField(p) => ETypeAbilityImpl::UnderscoreField(p),
            ERecord::Colon(p) => ETypeAbilityImpl::Colon(p),
            ERecord::Arrow(p) => ETypeAbilityImpl::Arrow(p),
            ERecord::Space(s, p) => ETypeAbilityImpl::Space(s, p),
            ERecord::Prefix(p) => ETypeAbilityImpl::Prefix(p),
            ERecord::QuestionMark(p) => ETypeAbilityImpl::QuestionMark(p),
            ERecord::SecondQuestionMark(p) => ETypeAbilityImpl::SecondQuestionMark(p),
            ERecord::Ampersand(p) => ETypeAbilityImpl::Ampersand(p),
            ERecord::Expr(e, p) => ETypeAbilityImpl::Expr(e, p),
        }
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
        let indent_text = "| ; : ! ".repeat(100);

        let cur_indent = INDENT.with(|i| *i.borrow());

        println!(
            "{:<5?}:{:<2} {}{:<50}",
            state.pos(),
            min_indent,
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
            "{:<5?}:{:<2} {}{:<50} {:<15} {:?}",
            state.pos(),
            min_indent,
            &indent_text[..cur_indent * 2],
            self.message,
            format!("{:?}", progress),
            value
        );

        res
    }
}

/// Allocates the output of the given parser and returns a reference to it.
/// This also happens if the given parser fails.
///
/// # Examples
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, allocated, word};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser_inner = word("hello", Problem::NotFound);
/// let alloc_parser = allocated(parser_inner);
///
/// // Success case
/// let (progress, output, state) = alloc_parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, &());
/// assert_eq!(state.pos(), Position::new(5));
///
/// // Error case
/// let (progress, err) = alloc_parser.parse(&arena, State::new("bye, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::NotFound(Position::zero()));
/// ```
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

/// Apply transform function to turn output of given parser into another parser.
/// Can be used to chain two parsers.
///
/// # Examples
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, and_then, word};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser1 = word("hello", Problem::NotFound);
/// let parser = and_then(parser1, move |p,b| word(", ", Problem::NotFound));
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos(), Position::new(7));
///
/// // Error case
/// let (progress, problem) = parser.parse(&arena, State::new("hello!! world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(problem, Problem::NotFound(Position::new(5)));
/// ```
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

/// Creates a new parser that can change its output based on a function.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, then, word};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// #     OddChar(Position),
/// # }
/// # let arena = Bump::new();
/// let parser_inner = word("hello", Problem::NotFound);
///
/// let parser = then(parser_inner,
///     |arena, new_state, progress, output| {
///         // arbitrary check
///         if new_state.pos().offset % 2 == 0 {
///             Ok((progress, output, new_state))
///         } else {
///             Err((Progress::NoProgress, Problem::OddChar(new_state.pos())))
///         }
///     }
/// );
///
/// let actual = parser.parse(&arena, State::new("hello, world".as_bytes()), 0);
/// assert!(actual.is_err());
/// ```
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

/// Matches a word/string exactly, useful when finding a keyword.
/// This only matches if the next char is whitespace, the start of a comment, or the end of a line.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, keyword};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = keyword("when", Problem::NotFound);
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("when".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos().offset, 4);
///
/// // Error case
/// let (progress, err) = parser.parse(&arena, State::new("whence".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::NotFound(Position::zero()));
/// ```
pub fn keyword<'a, ToError, E>(
    keyword_str: &'static str,
    if_error: ToError,
) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    move |_, mut state: State<'a>, _min_indent| {
        let width = keyword_str.len();

        if !state.bytes().starts_with(keyword_str.as_bytes()) {
            return Err((NoProgress, if_error(state.pos())));
        }

        // the next character should not be an identifier character
        // to prevent treating `whence` or `iffy` as keywords
        match state.bytes().get(width) {
            Some(
                b' ' | b'#' | b'\n' | b'\r' | b'\t' | b',' | b'(' | b')' | b'[' | b']' | b'{'
                | b'}' | b'"' | b'\'' | b'/' | b'\\' | b'+' | b'*' | b'%' | b'^' | b'&' | b'|'
                | b'<' | b'>' | b'=' | b'!' | b'~' | b'`' | b';' | b':' | b'?' | b'.' | b'@' | b'-',
            ) => {
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

/// Make the given parser optional, it can complete or not consume anything,
/// but it can't error with progress made.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, optional, word};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = optional(word("hello", Problem::NotFound));
///
/// // Parser completed case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, Some(()));
/// assert_eq!(state.pos().offset, 5);
///
/// // No progress case
/// let (progress, output, state) = parser.parse(&arena, State::new("bye, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(output, None);
/// assert_eq!(state.pos().offset, 0);
/// ```
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
// Using some combinators together results in combinatorial type explosion,
// this takes forever to compile. Using macros instead avoids this!

/// Wraps the output of the given parser in a [`Loc`](../roc_region/all/struct.Loc.html) struct,
/// to provide location information.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, loc};
/// # use roc_region::all::{Loc, Position};
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// # fn foo<'a>(arena: &'a Bump) {
/// let parser = loc(word("hello", Problem::NotFound));
///
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, Loc::new(0, 5, ()));
/// assert_eq!(state.pos().offset, 5);
/// # }
/// # foo(&arena);
/// ```
pub fn loc<'a, Output, E: 'a>(
    parser: impl Parser<'a, Output, E>,
) -> impl Parser<'a, Loc<Output>, E> {
    move |arena, state: crate::state::State<'a>, min_indent: u32| {
        let start = state.pos();

        match parser.parse(arena, state, min_indent) {
            Ok((progress, value, state)) => {
                let end = state.pos();
                let region = Region::new(start, end);

                Ok((progress, Loc { region, value }, state))
            }
            Err(err) => Err(err),
        }
    }
}

/// If the first one parses, ignore its output and move on to parse with the second one.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, skip_first};
/// # use roc_parse::ident::lowercase_ident;
/// # use bumpalo::Bump;
/// # let arena = Bump::new();
/// # fn foo<'a>(arena: &'a Bump) {
/// let parser = skip_first(
///    word("hello, ", |_| ()),
///    lowercase_ident()
/// );
///
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, "world");
/// assert_eq!(state.pos().offset, 12);
/// # }
/// # foo(&arena);
/// ```
pub fn skip_first<'a, P1, First, P2, Second, E>(p1: P1, p2: P2) -> impl Parser<'a, Second, E>
where
    P1: Parser<'a, First, E>,
    P2: Parser<'a, Second, E>,
    E: 'a,
{
    move |arena, state: crate::state::State<'a>, min_indent: u32| match p1
        .parse(arena, state, min_indent)
    {
        Ok((p1, _, state)) => match p2.parse(arena, state, min_indent) {
            Ok((p2, out2, state)) => Ok((p1.or(p2), out2, state)),
            Err((p2, fail)) => Err((p1.or(p2), fail)),
        },
        Err((progress, fail)) => Err((progress, fail)),
    }
}

/// If the first one parses, parse the second one; if it also parses, use the
/// output from the first one.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, skip_second};
/// # use roc_parse::ident::lowercase_ident;
/// # use bumpalo::Bump;
/// # let arena = Bump::new();
/// # fn foo<'a>(arena: &'a Bump) {
/// let parser = skip_second(
///    lowercase_ident(),
///    word(", world", |_| ())
/// );
///
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, "hello");
/// assert_eq!(state.pos().offset, 12);
/// # }
/// # foo(&arena);
/// ```
pub fn skip_second<'a, P1, First, P2, Second, E>(p1: P1, p2: P2) -> impl Parser<'a, First, E>
where
    E: 'a,
    P1: Parser<'a, First, E>,
    P2: Parser<'a, Second, E>,
{
    move |arena, state: crate::state::State<'a>, min_indent: u32| match p1
        .parse(arena, state, min_indent)
    {
        Ok((p1, out1, state)) => match p2.parse(arena, state, min_indent) {
            Ok((p2, _, state)) => Ok((p1.or(p2), out1, state)),
            Err((p2, fail)) => Err((p1.or(p2), fail)),
        },
        Err((progress, fail)) => Err((progress, fail)),
    }
}

pub fn collection_inner<'a, Elem: 'a + crate::ast::Spaceable<'a> + Clone, E: 'a + SpaceProblem>(
    elem: impl Parser<'a, Loc<Elem>, E> + 'a,
    delimiter: impl Parser<'a, (), E>,
    space_before: impl Fn(&'a Elem, &'a [crate::ast::CommentOrNewline<'a>]) -> Elem,
) -> impl Parser<'a, crate::ast::Collection<'a, Loc<Elem>>, E> {
    map_with_arena(
        and(
            and(
                crate::blankspace::spaces(),
                trailing_sep_by0(
                    delimiter,
                    crate::blankspace::spaces_before_optional_after(elem),
                ),
            ),
            crate::blankspace::spaces(),
        ),
        #[allow(clippy::type_complexity)]
        move |arena: &'a bumpalo::Bump,
              out: (
            (
                &'a [crate::ast::CommentOrNewline<'a>],
                bumpalo::collections::Vec<'a, Loc<Elem>>,
            ),
            &'a [crate::ast::CommentOrNewline<'a>],
        )| {
            let ((spaces, mut parsed_elems), mut final_comments) = out;

            if !spaces.is_empty() {
                if let Some(first) = parsed_elems.first_mut() {
                    first.value = space_before(arena.alloc(first.value.clone()), spaces);
                } else {
                    debug_assert!(final_comments.is_empty());
                    final_comments = spaces;
                }
            }

            crate::ast::Collection::with_items_and_comments(
                arena,
                parsed_elems.into_bump_slice(),
                final_comments,
            )
        },
    )
}

pub fn collection_trailing_sep_e<
    'a,
    Elem: 'a + crate::ast::Spaceable<'a> + Clone,
    E: 'a + SpaceProblem,
>(
    opening_brace: impl Parser<'a, (), E>,
    elem: impl Parser<'a, Loc<Elem>, E> + 'a,
    delimiter: impl Parser<'a, (), E>,
    closing_brace: impl Parser<'a, (), E>,
    space_before: impl Fn(&'a Elem, &'a [crate::ast::CommentOrNewline<'a>]) -> Elem,
) -> impl Parser<'a, crate::ast::Collection<'a, Loc<Elem>>, E> {
    between(
        opening_brace,
        reset_min_indent(collection_inner(elem, delimiter, space_before)),
        closing_brace,
    )
}

/// Creates a parser that always succeeds with the given argument as output.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, succeed};
/// # use bumpalo::Bump;
/// # let arena = Bump::new();
/// # fn foo<'a>(arena: &'a Bump) {
/// let parser = succeed("different");
///
/// let (progress, output, state) = Parser::<&'a str,()>::parse(&parser, &arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(output, "different");
/// assert_eq!(state.pos().offset, 0);
/// # }
/// # foo(&arena);
/// ```
pub fn succeed<'a, T: Clone, E: 'a>(value: T) -> impl Parser<'a, T, E> {
    move |_arena: &'a bumpalo::Bump, state: crate::state::State<'a>, _min_indent: u32| {
        Ok((NoProgress, value.clone(), state))
    }
}

/// Creates a parser that always fails.
/// If the given parser succeeds, the error is customized with the given function.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, fail_when};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// #     OtherProblem(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = fail_when(Problem::OtherProblem, word("hello", Problem::NotFound));
///
/// // When given parser succeeds:
/// let (progress, err) = Parser::<(), Problem>::parse(&parser, &arena, State::new("hello, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(err, Problem::OtherProblem(Position::new(0)));
///
/// // When given parser errors:
/// let (progress, err) = Parser::<(), Problem>::parse(&parser, &arena, State::new("bye, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::NotFound(Position::new(0)));
/// ```
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

/// Creates a parser that always fails using the given error function.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, fail};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = fail(Problem::NotFound);
///
/// let (progress, err) = Parser::<(), Problem>::parse(&parser, &arena, State::new("hello, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::NotFound(Position::new(0)));
/// ```
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

/// Creates a parser that fails if the next byte is the given byte.
pub fn error_on_byte<'a, T, E, F>(byte_to_match: u8, to_error: F) -> impl Parser<'a, T, E>
where
    T: 'a,
    E: 'a,
    F: Fn(Position) -> E,
{
    debug_assert_ne!(byte_to_match, b'\n');

    move |_arena: &'a Bump, state: State<'a>, _min_indent: u32| match state.bytes().first() {
        Some(x) if *x == byte_to_match => Err((MadeProgress, to_error(state.pos()))),
        _ => Err((NoProgress, to_error(state.pos()))),
    }
}

/// Runs two parsers in succession. If both parsers succeed, the output is a tuple of both outputs.
/// Both parsers must have the same error type.
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, and, word};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser1 = word("hello", Problem::NotFound);
/// let parser2 = word(", ", Problem::NotFound);
/// let parser = and(parser1, parser2);
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ((),()));
/// assert_eq!(state.pos(), Position::new(7));
///
/// // Error case
/// let (progress, err) = parser.parse(&arena, State::new("hello!! world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(err, Problem::NotFound(Position::new(5)));
/// ```
pub fn and<'a, Output1, Output2, E: 'a>(
    p1: impl Parser<'a, Output1, E>,
    p2: impl Parser<'a, Output2, E>,
) -> impl Parser<'a, (Output1, Output2), E> {
    move |arena: &'a bumpalo::Bump, state: crate::state::State<'a>, min_indent: u32| match p1
        .parse(arena, state, min_indent)
    {
        Ok((p1, out1, state)) => match p2.parse(arena, state, min_indent) {
            Ok((p2, out2, state)) => Ok((p1.or(p2), (out1, out2), state)),
            Err((p2, fail)) => Err((p1.or(p2), fail)),
        },
        Err((progress, fail)) => Err((progress, fail)),
    }
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

/// Similar to [`skip_first`], but we modify the `min_indent` of the second
/// parser (`parser`) to be 1 greater than the `line_indent()` at the start of
/// the first parser (`before`).
pub fn indented_seq_skip_first<'a, O, E: 'a>(
    before: impl Parser<'a, (), E>,
    parser: impl Parser<'a, O, E>,
) -> impl Parser<'a, O, E> {
    move |arena: &'a bumpalo::Bump, state: crate::state::State<'a>, _min_indent: u32| {
        let start_indent = state.line_indent();

        // TODO: we should account for min_indent here, but this doesn't currently work
        // because min_indent is sometimes larger than it really should be, which is in turn
        // due to uses of `increment_indent`.
        //
        // let p1_indent = std::cmp::max(start_indent, min_indent);

        let p1_indent = start_indent;
        let p2_indent = p1_indent + 1;

        match before.parse(arena, state, p1_indent) {
            Ok((p1, (), state)) => match parser.parse(arena, state, p2_indent) {
                Ok((p2, out2, state)) => Ok((p1.or(p2), out2, state)),
                Err((p2, fail)) => Err((p1.or(p2), fail)),
            },
            Err((progress, fail)) => Err((progress, fail)),
        }
    }
}

/// Similar to `and`, but we modify the min_indent of the second parser to be
/// 1 greater than the line_indent() at the start of the first parser.
pub fn indented_seq<'a, Output1, Output2, E: 'a>(
    p1: impl Parser<'a, Output1, E>,
    p2: impl Parser<'a, Output2, E>,
) -> impl Parser<'a, (Output1, Output2), E> {
    move |arena: &'a bumpalo::Bump, state: crate::state::State<'a>, _min_indent: u32| {
        let start_indent = state.line_indent();

        // TODO: we should account for min_indent here, but this doesn't currently work
        // because min_indent is sometimes larger than it really should be, which is in turn
        // due to uses of `increment_indent`.
        //
        // let p1_indent = std::cmp::max(start_indent, min_indent);

        let p1_indent = start_indent;
        let p2_indent = p1_indent + 1;

        match p1.parse(arena, state, p1_indent) {
            Ok((p1, out1, state)) => match p2.parse(arena, state, p2_indent) {
                Ok((p2, out2, state)) => Ok((p1.or(p2), (out1, out2), state)),
                Err((p2, fail)) => Err((p1.or(p2), fail)),
            },
            Err((progress, fail)) => Err((progress, fail)),
        }
    }
}

/// Similar to [`and`], but we modify the `min_indent` of the second parser to be
/// 1 greater than the `column()` at the start of the first parser.
pub fn absolute_indented_seq<'a, Output1, Output2, E: 'a>(
    p1: impl Parser<'a, Output1, E>,
    p2: impl Parser<'a, Output2, E>,
) -> impl Parser<'a, (Output1, Output2), E> {
    move |arena: &'a bumpalo::Bump, state: crate::state::State<'a>, _min_indent: u32| {
        let start_indent = state.column();

        let p1_indent = start_indent;
        let p2_indent = p1_indent + 1;

        match p1.parse(arena, state, p1_indent) {
            Ok((p1, out1, state)) => match p2.parse(arena, state, p2_indent) {
                Ok((p2, out2, state)) => Ok((p1.or(p2), (out1, out2), state)),
                Err((p2, fail)) => Err((p1.or(p2), fail)),
            },
            Err((progress, fail)) => Err((progress, fail)),
        }
    }
}

/// Returns the result of the first parser that makes progress, even if it failed.
/// If no parsers make progress, the last parser's result is returned.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, byte};
/// # use roc_region::all::Position;
/// # use roc_parse::one_of;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// # fn foo<'a>(arena: &'a Bump) {
/// let parser1 = one_of!(
///     word("hello", Problem::NotFound),
///     byte(b'h', Problem::NotFound)
/// );
/// let (progress, output, state) = parser1.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos().offset, 5);
///
/// let parser2 = one_of!(
///     // swapped the order of the parsers
///     byte(b'h', Problem::NotFound),
///     word("hello", Problem::NotFound)
/// );
/// let (progress, output, state) = parser2.parse(&arena, State::new("hello! world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos().offset, 1);
/// # }
/// # foo(&arena);
/// ```
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

pub fn reset_min_indent<'a, P, T, X: 'a>(parser: P) -> impl Parser<'a, T, X>
where
    P: Parser<'a, T, X>,
{
    move |arena, state, _min_indent| parser.parse(arena, state, 0)
}

pub fn capture_line_indent<'a, X: 'a>() -> impl Parser<'a, u32, X> {
    move |_arena, state: State<'a>, _min_indent| Ok((NoProgress, state.line_indent(), state))
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

/// Transforms a possible error, like `map_err` in Rust.
/// It has no effect if the given parser succeeds.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, specialize_err};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// #     Other(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = specialize_err(
///     |_prev_err, pos| Problem::Other(pos),
///     word("bye", Problem::NotFound)
/// );
///
/// let (progress, err) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::Other(Position::new(0)));
/// ```
pub fn specialize_err<'a, F, P, T, X, Y>(map_error: F, parser: P) -> impl Parser<'a, T, Y>
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

/// Transforms a possible error, like `map_err` in Rust.
/// Similar to [`specialize_err`], but the error is arena allocated, and the
/// mapping function receives a reference to the error.
/// It has no effect if the inner parser succeeds.
///
/// # Examples
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, specialize_err_ref};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// #     Other(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = specialize_err_ref(
///     |_prev_err, pos| Problem::Other(pos),
///     word("bye", Problem::NotFound)
/// );
///
/// let (progress, err) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::Other(Position::new(0)));
/// ```
pub fn specialize_err_ref<'a, F, P, T, X, Y>(map_error: F, parser: P) -> impl Parser<'a, T, Y>
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

/// Matches an entire `str` at the beginning of the state's bytes and moves the state's position forward if it succeeds.
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = word("hello", Problem::NotFound);
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos(), Position::new(5));
///
/// // Error case
/// let (progress, problem) = parser.parse(&arena, State::new("bye, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(problem, Problem::NotFound(Position::zero()));
///
/// let (progress, problem) = parser.parse(&arena, State::new("world, hello".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(problem, Problem::NotFound(Position::zero()));
/// ```
pub fn word<'a, ToError, E>(word: &'static str, to_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    debug_assert!(!word.contains('\n'));

    move |_arena: &'a Bump, state: State<'a>, _min_indent: u32| {
        if state.bytes().starts_with(word.as_bytes()) {
            let state = state.advance(word.len());
            Ok((MadeProgress, (), state))
        } else {
            Err((NoProgress, to_error(state.pos())))
        }
    }
}

/// Matches a single `u8` and moves the state's position forward if it succeeds.
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, byte};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = byte(b'h', Problem::NotFound);
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos(), Position::new(1));
///
/// // Error case
/// let (progress, problem) = parser.parse(&arena, State::new("bye, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(problem, Problem::NotFound(Position::zero()));
/// ```
pub fn byte<'a, ToError, E>(byte_to_match: u8, to_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    debug_assert_ne!(byte_to_match, b'\n');

    move |_arena: &'a Bump, state: State<'a>, _min_indent: u32| match state.bytes().first() {
        Some(x) if *x == byte_to_match => {
            let state = state.advance(1);
            Ok((MadeProgress, (), state))
        }
        _ => Err((NoProgress, to_error(state.pos()))),
    }
}

/// Matches a single `u8` and moves the state's position forward if it succeeds.
/// This parser will fail if it is a lower indentation level than it should be.
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, byte, byte_indent};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// #     WrongIndentLevel(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = byte_indent(b'h', Problem::WrongIndentLevel);
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos(), Position::new(1));
///
/// // Error case
/// let state = State::new(" hello, world".as_bytes());
/// let _ = byte(b' ', Problem::NotFound).parse(&arena, state.clone(), 0).unwrap();
/// let (progress, problem) = parser.parse(&arena, state, 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(problem, Problem::WrongIndentLevel(Position::zero()));
/// ```
pub fn byte_indent<'a, ToError, E>(byte_to_match: u8, to_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    debug_assert_ne!(byte_to_match, b'\n');

    move |_arena: &'a Bump, state: State<'a>, min_indent: u32| {
        if min_indent > state.column() {
            return Err((NoProgress, to_error(state.pos())));
        }

        match state.bytes().first() {
            Some(x) if *x == byte_to_match => {
                let state = state.advance(1);
                Ok((MadeProgress, (), state))
            }
            _ => Err((NoProgress, to_error(state.pos()))),
        }
    }
}

/// Matches two `u8` in a row.
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, two_bytes};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = two_bytes(b'h', b'e', Problem::NotFound);
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos(), Position::new(2));
///
/// // Error case
/// let (progress, problem) = parser.parse(&arena, State::new("hi, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(problem, Problem::NotFound(Position::zero()));
/// ```
pub fn two_bytes<'a, ToError, E>(
    byte_1: u8,
    byte_2: u8,
    to_error: ToError,
) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    debug_assert_ne!(byte_1, b'\n');
    debug_assert_ne!(byte_2, b'\n');

    let needle = [byte_1, byte_2];

    move |_arena: &'a Bump, state: State<'a>, _min_indent: u32| {
        if state.bytes().starts_with(&needle) {
            let state = state.advance(2);
            Ok((MadeProgress, (), state))
        } else {
            Err((NoProgress, to_error(state.pos())))
        }
    }
}

/// Matches three `u8` in a row.
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, three_bytes};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = three_bytes(b'h', b'e', b'l', Problem::NotFound);
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos(), Position::new(3));
///
/// // Error case
/// let (progress, err) = parser.parse(&arena, State::new("hi, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::NotFound(Position::zero()));
/// ```
pub fn three_bytes<'a, ToError, E>(
    byte_1: u8,
    byte_2: u8,
    byte_3: u8,
    to_error: ToError,
) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    debug_assert_ne!(byte_1, b'\n');
    debug_assert_ne!(byte_2, b'\n');
    debug_assert_ne!(byte_3, b'\n');

    let needle = [byte_1, byte_2, byte_3];

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
macro_rules! byte_check_indent {
    ($byte_to_match:expr, $problem:expr, $min_indent:expr, $indent_problem:expr) => {
        $crate::parser::and(
            byte($byte_to_match, $problem),
            $crate::parser::check_indent($min_indent, $indent_problem),
        )
    };
}

/// transform the `Ok` result of a parser
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, map};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = map(
///     word("hello", Problem::NotFound),
///     |_output| "new output!"
/// );
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, "new output!");
/// assert_eq!(state.pos(), Position::new(5));
///
/// // Error case
/// let (progress, err) = parser.parse(&arena, State::new("bye, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::NotFound(Position::zero()));
/// ```
pub fn map<'a, Output, MappedOutput, E: 'a>(
    parser: impl Parser<'a, Output, E>,
    transform: impl Fn(Output) -> MappedOutput,
) -> impl Parser<'a, MappedOutput, E> {
    move |arena, state, min_indent| {
        parser
            .parse(arena, state, min_indent)
            .map(|(progress, output, next_state)| (progress, transform(output), next_state))
    }
}

/// Applies the parser as many times as possible.
/// This parser will only fail if the given parser makes partial progress.
pub fn zero_or_more<'a, Output, E: 'a>(
    parser: impl Parser<'a, Output, E>,
) -> impl Parser<'a, bumpalo::collections::Vec<'a, Output>, E> {
    move |arena, state: State<'a>, min_indent: u32| {
        let original_state = state.clone();

        let start_bytes_len = state.bytes().len();

        match parser.parse(arena, state, min_indent) {
            Ok((_, first_output, next_state)) => {
                let mut state = next_state;
                let mut buf = Vec::with_capacity_in(1, arena);

                buf.push(first_output);

                loop {
                    let old_state = state.clone();
                    match parser.parse(arena, state, min_indent) {
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
}

/// Creates a parser that matches one or more times.
/// Will fail if the given parser fails to match or matches partially.
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, one_or_more};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// # fn foo<'a>(arena: &'a Bump) {
/// let parser = one_or_more(
///     word("hello, ", Problem::NotFound),
///     Problem::NotFound
/// );
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, bumpalo::vec![in &arena; (),()]);
/// assert_eq!(state.pos(), Position::new(14));
///
/// // Error case
/// let (progress, err) = parser.parse(&arena, State::new("bye, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::NotFound(Position::zero()));
/// # }
/// # foo(&arena);
/// ```
pub fn one_or_more<'a, Output, E: 'a>(
    parser: impl Parser<'a, Output, E>,
    to_error: impl Fn(Position) -> E,
) -> impl Parser<'a, bumpalo::collections::Vec<'a, Output>, E> {
    move |arena, state: State<'a>, min_indent: u32| match parser.parse(
        arena,
        state.clone(),
        min_indent,
    ) {
        Ok((_, first_output, next_state)) => {
            let mut state = next_state;
            let mut buf = Vec::with_capacity_in(1, arena);

            buf.push(first_output);

            loop {
                let old_state = state.clone();
                match parser.parse(arena, state, min_indent) {
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
        Err((progress, _)) => Err((progress, to_error(state.pos()))),
    }
}

/// Creates a parser that debug prints the result of parsing.
/// It doesn't change the given parser at all,
/// useful for inspecting a parser during development.
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word};
/// # use roc_region::all::Position;
/// # use roc_parse::debug;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// # fn foo<'a>(arena: &'a Bump) {
/// let parser = debug!(
///     word("hello", Problem::NotFound)
/// );
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos().offset, 5);
/// # }
/// # foo(&arena);
/// ```
#[macro_export]
macro_rules! debug {
    ($parser:expr) => {
        move |arena, state: $crate::state::State<'a>, min_indent: u32| {
            dbg!($parser.parse(arena, state, min_indent))
        }
    };
}

/// Matches either of the two given parsers.
/// If the first parser succeeds, its result is used,
/// otherwise, the second parser's result is used.
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, either, Either};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// # fn foo<'a>(arena: &'a Bump) {
/// let parser = either(
///     word("hello", Problem::NotFound),
///     word("bye", Problem::NotFound)
/// );
///
/// // Success cases
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, Either::First(()));
/// assert_eq!(state.pos().offset, 5);
///
/// let (progress, output, state) = parser.parse(&arena, State::new("bye, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, Either::Second(()));
/// assert_eq!(state.pos().offset, 3);
///
/// // Error case
/// let (progress, err) = parser.parse(&arena, State::new("later, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::NotFound(Position::zero()));
/// # }
/// # foo(&arena);
/// ```
pub fn either<'a, OutputLeft, OutputRight, E: 'a>(
    p1: impl Parser<'a, OutputLeft, E>,
    p2: impl Parser<'a, OutputRight, E>,
) -> impl Parser<'a, Either<OutputLeft, OutputRight>, E> {
    move |arena: &'a bumpalo::Bump, state: crate::state::State<'a>, min_indent: u32| {
        let original_state = state.clone();
        match p1.parse(arena, state, min_indent) {
            Ok((progress, output, state)) => {
                Ok((progress, crate::parser::Either::First(output), state))
            }
            Err((NoProgress, _)) => match p2.parse(arena, original_state.clone(), min_indent) {
                Ok((progress, output, state)) => {
                    Ok((progress, crate::parser::Either::Second(output), state))
                }
                Err((progress, fail)) => Err((progress, fail)),
            },
            Err((MadeProgress, fail)) => Err((MadeProgress, fail)),
        }
    }
}

/// Given three parsers, parse them all but ignore the output of the first and last one.
/// Useful for parsing things between two braces (e.g. parentheses).
///
/// If any of the three parsers error, this will error.
///
/// # Example
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, byte, between};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// # fn foo<'a>(arena: &'a Bump) {
/// let parser = between(
///     byte(b'(', Problem::NotFound),
///     word("hello", Problem::NotFound),
///     byte(b')', Problem::NotFound)
/// );
/// let (progress, output, state) = parser.parse(&arena, State::new("(hello), world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos().offset, 7);
/// # }
/// # foo(&arena);
/// ```
pub fn between<'a, Before, Inner, After, Err: 'a>(
    opening_brace: impl Parser<'a, Before, Err>,
    inner: impl Parser<'a, Inner, Err>,
    closing_brace: impl Parser<'a, After, Err>,
) -> impl Parser<'a, Inner, Err> {
    skip_first(opening_brace, skip_second(inner, closing_brace))
}

/// Maps/transforms the `Ok` result of parsing using the given function.
/// Similar to [`map`], but the transform function also takes a bump allocator.
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, map_with_arena};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = map_with_arena(
///     word("hello", Problem::NotFound),
///     |_arena, _output| "new output!"
/// );
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::MadeProgress);
/// assert_eq!(output, "new output!");
/// assert_eq!(state.pos(), Position::new(5));
///
/// // Error Case
/// let (progress, err) = parser.parse(&arena, State::new("bye, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::NotFound(Position::zero()));
/// ```
#[inline(always)]
pub fn map_with_arena<'a, P, F, Before, After, E>(
    parser: P,
    transform: F,
) -> impl Parser<'a, After, E>
where
    P: Parser<'a, Before, E>,
    F: Fn(&'a Bump, Before) -> After,
    E: 'a,
{
    move |arena, state, min_indent| {
        parser
            .parse(arena, state, min_indent)
            .map(|(progress, output, next_state)| (progress, transform(arena, output), next_state))
    }
}

/// Creates a new parser that does not progress but still forwards the output of
/// the given parser if it succeeds.
///
/// # Example
///
/// ```
/// # #![forbid(unused_imports)]
/// # use roc_parse::state::State;
/// # use crate::roc_parse::parser::{Parser, Progress, word, backtrackable};
/// # use roc_region::all::Position;
/// # use bumpalo::Bump;
/// # #[derive(Debug, PartialEq)]
/// # enum Problem {
/// #     NotFound(Position),
/// # }
/// # let arena = Bump::new();
/// let parser = backtrackable(
///     word("hello", Problem::NotFound),
/// );
///
/// // Success case
/// let (progress, output, state) = parser.parse(&arena, State::new("hello, world".as_bytes()), 0).unwrap();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(output, ());
/// assert_eq!(state.pos().offset, 5);
///
/// // Error Case
/// let (progress, err) = parser.parse(&arena, State::new("bye, world".as_bytes()), 0).unwrap_err();
/// assert_eq!(progress, Progress::NoProgress);
/// assert_eq!(err, Problem::NotFound(Position::zero()));
/// ```
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
