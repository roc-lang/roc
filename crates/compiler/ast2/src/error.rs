use roc_region::all::{Position, Region};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EIdent {
    Start(Position),
    Space(EInput, Position),

    Underscore(Position),
    QualifiedTag(Position),
    WeirdAccessor(Position),
    WeirdDotAccess(Position),
    WeirdDotQualified(Position),
    StrayDot(Position),
    BadOpaqueRef(Position),
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

    Space(EInput, Position),
    Start(Position),
    ModuleName(Position),
    AppName(EString<'a>, Position),
    PackageName(EPackageName<'a>, Position),
    PlatformName(EPackageName<'a>, Position),
    IndentStart(Position),

    InconsistentModuleName(Region),
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
    Space(EInput, Position),
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
    Space(EInput, Position),
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
    Space(EInput, Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypedIdent<'a> {
    Space(EInput, Position),
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
    Space(EInput, Position),
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
    Space(EInput, Position),
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
    Space(EInput, Position),
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
    Space(EInput, Position),
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
    Space(EInput, Position),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EInput {
    HasTab,
    HasMisplacedCarriageReturn,
    HasAsciiControl,
    ///
    TooManyLines,
    ///
    ///
    BadUtf8,
}

pub trait SpaceProblem: std::fmt::Debug {
    fn space_problem(e: EInput, pos: Position) -> Self;
}

macro_rules! impl_space_problem {
    ($($name:ident $(< $lt:tt >)?),*) => {
        $(
            impl $(< $lt >)? SpaceProblem for $name $(< $lt >)? {
                fn space_problem(e: EInput, pos: Position) -> Self {
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
    EClosure<'a>,
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
    ETypeAbilityImpl<'a>,
    EWhen<'a>,
    EAbility<'a>,
    EPatternInParens<'a>,
    EPatternRecord<'a>,
    EPatternList<'a>,
    ETuple<'a>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EExpr<'a> {
    TrailingOperator(Position),

    Start(Position),
    End(Position),
    BadExprEnd(Position),
    Space(EInput, Position),

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
    BackpassComma(Position),
    BackpassArrow(Position),

    When(EWhen<'a>, Position),
    If(EIf<'a>, Position),

    Expect(EExpect<'a>, Position),
    Dbg(EExpect<'a>, Position),

    Closure(EClosure<'a>, Position),
    Underscore(Position),
    Crash(Position),

    InParens(EInParens<'a>, Position),
    Record(ERecord<'a>, Position),
    Tuple(ETuple<'a>, Position),
    Str(EString<'a>, Position),
    SingleQuote(EString<'a>, Position),
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

    Space(EInput, Position),
    EndlessSingle(Position),
    EndlessMulti(Position),
    UnknownEscape(Position),
    Format(&'a EExpr<'a>, Position),
    FormatEnd(Position),
    MultilineInsufficientIndent(Position),
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

    Space(EInput, Position),

    IndentOpen(Position),
    IndentColon(Position),
    IndentBar(Position),
    IndentAmpersand(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETuple<'a> {
    // Empty tuples are not allowed
    Empty(Position),

    // Single element tuples are not allowed
    Single(Position),

    End(Position),
    Open(Position),

    Updateable(Position),
    Field(Position),
    Colon(Position),
    QuestionMark(Position),
    Bar(Position),
    Ampersand(Position),

    Expr(&'a EExpr<'a>, Position),

    Space(EInput, Position),

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

    /// Empty parens, e.g. () is not allowed
    Empty(Position),

    ///
    Expr(&'a EExpr<'a>, Position),

    ///
    Space(EInput, Position),
    ///
    IndentOpen(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EClosure<'a> {
    Space(EInput, Position),
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
    Space(EInput, Position),

    Expr(&'a EExpr<'a>, Position),

    IndentOpen(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EWhen<'a> {
    Space(EInput, Position),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EAbility<'a> {
    Space(EInput, Position),
    Type(EType<'a>, Position),

    DemandAlignment(i32, Position),
    DemandName(Position),
    DemandColon(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EIf<'a> {
    Space(EInput, Position),
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
    Space(EInput, Position),
    Dbg(Position),
    Expect(Position),
    Condition(&'a EExpr<'a>, Position),
    Continuation(&'a EExpr<'a>, Position),
    IndentCondition(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPattern<'a> {
    Record(EPatternRecord<'a>, Position),
    List(EPatternList<'a>, Position),
    Underscore(Position),
    NotAPattern(Position),

    Start(Position),
    End(Position),
    Space(EInput, Position),

    PInParens(EPatternInParens<'a>, Position),
    NumLiteral(ENumber, Position),

    IndentStart(Position),
    IndentEnd(Position),
    AsIndentStart(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPatternRecord<'a> {
    End(Position),
    Open(Position),

    Field(Position),
    Colon(Position),
    Optional(Position),

    Pattern(&'a EPattern<'a>, Position),
    Expr(&'a EExpr<'a>, Position),

    Space(EInput, Position),

    IndentOpen(Position),
    IndentColon(Position),
    IndentOptional(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPatternList<'a> {
    End(Position),
    Open(Position),

    Rest(Position),
    Pattern(&'a EPattern<'a>, Position),

    Space(EInput, Position),

    IndentOpen(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPatternInParens<'a> {
    Empty(Position),
    End(Position),
    Open(Position),
    Pattern(&'a EPattern<'a>, Position),

    Space(EInput, Position),
    IndentOpen(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EType<'a> {
    Space(EInput, Position),

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
    THasClause(Position),
    TAbilityImpl(ETypeAbilityImpl<'a>, Position),
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

    Space(EInput, Position),

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

    Space(EInput, Position),

    IndentOpen(Position),
    IndentEnd(Position),
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
    Space(EInput, Position),
    ///
    IndentOpen(Position),
    IndentEnd(Position),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeApply {
    ///
    StartNotUppercase(Position),
    End(Position),
    Space(EInput, Position),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeAbilityImpl<'a> {
    End(Position),
    Open(Position),

    Field(Position),
    Colon(Position),
    Optional(Position),
    Type(&'a EType<'a>, Position),

    Space(EInput, Position),

    IndentOpen(Position),
    IndentColon(Position),
    IndentOptional(Position),
    IndentEnd(Position),
    Updateable(Position),
    QuestionMark(Position),
    Bar(Position),
    Ampersand(Position),
    Expr(&'a EExpr<'a>, Position),
    IndentBar(Position),
    IndentAmpersand(Position),
}

impl<'a> From<ERecord<'a>> for ETypeAbilityImpl<'a> {
    fn from(e: ERecord<'a>) -> Self {
        match e {
            ERecord::End(p) => ETypeAbilityImpl::End(p),
            ERecord::Open(p) => ETypeAbilityImpl::Open(p),
            ERecord::Field(p) => ETypeAbilityImpl::Field(p),
            ERecord::Colon(p) => ETypeAbilityImpl::Colon(p),
            ERecord::Space(s, p) => ETypeAbilityImpl::Space(s, p),
            ERecord::IndentOpen(p) => ETypeAbilityImpl::IndentOpen(p),
            ERecord::IndentColon(p) => ETypeAbilityImpl::IndentColon(p),
            ERecord::IndentEnd(p) => ETypeAbilityImpl::IndentEnd(p),
            ERecord::Updateable(p) => ETypeAbilityImpl::Updateable(p),
            ERecord::QuestionMark(p) => ETypeAbilityImpl::QuestionMark(p),
            ERecord::Bar(p) => ETypeAbilityImpl::Bar(p),
            ERecord::Ampersand(p) => ETypeAbilityImpl::Ampersand(p),
            ERecord::Expr(e, p) => ETypeAbilityImpl::Expr(e, p),
            ERecord::IndentBar(p) => ETypeAbilityImpl::IndentBar(p),
            ERecord::IndentAmpersand(p) => ETypeAbilityImpl::IndentAmpersand(p),
        }
    }
}
