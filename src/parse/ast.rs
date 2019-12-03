use crate::ident::UnqualifiedIdent;
use crate::module::ModuleName;
use crate::operator::CalledVia;
use crate::operator::{BinOp, UnaryOp};
use crate::parse::ident::Ident;
use crate::region::{Loc, Region};
use bumpalo::collections::String;
use bumpalo::collections::Vec;
use bumpalo::Bump;

#[derive(Clone, Debug, PartialEq)]
pub enum Module<'a> {
    Interface { header: InterfaceHeader<'a> },
    App { header: AppHeader<'a> },
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceHeader<'a> {
    pub name: Loc<ModuleName<'a>>,
    pub exposes: Vec<'a, Loc<ExposesEntry<'a>>>,
    pub imports: Vec<'a, Loc<ImportsEntry<'a>>>,

    // Potential comments and newlines - these will typically all be empty.
    pub after_interface: &'a [CommentOrNewline<'a>],
    pub before_exposes: &'a [CommentOrNewline<'a>],
    pub after_exposes: &'a [CommentOrNewline<'a>],
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppHeader<'a> {
    pub imports: Vec<'a, Loc<ImportsEntry<'a>>>,

    // Potential comments and newlines - these will typically all be empty.
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExposesEntry<'a> {
    /// e.g. `Task`
    Ident(UnqualifiedIdent<'a>),

    // Spaces
    SpaceBefore(&'a ExposesEntry<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a ExposesEntry<'a>, &'a [CommentOrNewline<'a>]),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImportsEntry<'a> {
    /// e.g. `Task` or `Task.{ Task, after }`
    Module(ModuleName<'a>, Vec<'a, Loc<ExposesEntry<'a>>>),

    // Spaces
    SpaceBefore(&'a ImportsEntry<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a ImportsEntry<'a>, &'a [CommentOrNewline<'a>]),
}

/// An optional qualifier (the `Foo.Bar` in `Foo.Bar.baz`).
/// If module_parts is empty, this is unqualified.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MaybeQualified<'a, Val> {
    pub module_parts: &'a [&'a str],
    pub value: Val,
}

impl<'a> MaybeQualified<'a, &'a str> {
    pub fn len(&self) -> usize {
        let mut answer = self.value.len();

        for part in self.module_parts {
            answer += part.len();
        }

        answer
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'a> MaybeQualified<'a, &'a [&'a str]> {
    pub fn len(&self) -> usize {
        let mut answer = 0;

        for module_part in self.module_parts {
            answer += module_part.len();
        }

        for value_part in self.module_parts {
            answer += value_part.len();
        }

        answer
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// A parsed expression. This uses lifetimes extensively for two reasons:
///
/// 1. It uses Bump::alloc for all allocations, which returns a reference.
/// 2. It often stores references into the input string instead of allocating.
///
/// This dramatically reduces allocations during parsing. Once parsing is done,
/// we move on to canonicalization, which often needs to allocate more because
/// it's doing things like turning local variables into fully qualified symbols.
/// Once canonicalization is done, the arena and the input string get dropped.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    // Number Literals
    Float(&'a str),
    Int(&'a str),
    HexInt(&'a str),
    OctalInt(&'a str),
    BinaryInt(&'a str),

    // String Literals
    Str(&'a str),
    BlockStr(&'a [&'a str]),
    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access(&'a Expr<'a>, UnqualifiedIdent<'a>),
    /// e.g. `.foo`
    AccessorFunction(UnqualifiedIdent<'a>),

    // Collection Literals
    List(Vec<'a, &'a Loc<Expr<'a>>>),
    Record(Vec<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    // Lookups
    Var(&'a [&'a str], &'a str),

    // Tags
    GlobalTag(&'a str),
    PrivateTag(&'a str),

    // Pattern Matching
    Closure(&'a Vec<'a, Loc<Pattern<'a>>>, &'a Loc<Expr<'a>>),
    /// Multiple defs in a row
    Defs(Vec<'a, &'a Loc<Def<'a>>>, &'a Loc<Expr<'a>>),

    // Application
    /// To apply by name, do Apply(Var(...), ...)
    /// To apply a tag by name, do Apply(Tag(...), ...)
    Apply(&'a Loc<Expr<'a>>, Vec<'a, &'a Loc<Expr<'a>>>, CalledVia),
    BinOp(&'a (Loc<Expr<'a>>, Loc<BinOp>, Loc<Expr<'a>>)),
    UnaryOp(&'a Loc<Expr<'a>>, Loc<UnaryOp>),

    // Conditionals
    If(&'a (Loc<Expr<'a>>, Loc<Expr<'a>>, Loc<Expr<'a>>)),
    Case(
        &'a Loc<Expr<'a>>,
        Vec<'a, &'a (Loc<Pattern<'a>>, Loc<Expr<'a>>)>,
    ),

    // Blank Space (e.g. comments, spaces, newlines) before or after an expression.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    ParensAround(&'a Expr<'a>),

    /// This is used only to avoid cloning when reordering expressions (e.g. in desugar()).
    /// It lets us take an (&Expr) and create a plain (Expr) from it.
    Nested(&'a Expr<'a>),

    // Problems
    MalformedIdent(&'a str),
    MalformedClosure,
    // Both operators were non-associative, e.g. (True == False == False).
    // We should tell the author to disambiguate by grouping them with parens.
    PrecedenceConflict(Loc<BinOp>, Loc<BinOp>, &'a Loc<Expr<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Def<'a> {
    // TODO in canonicalization, validate the pattern; only certain patterns
    // are allowed in annotations.
    Annotation(Loc<Pattern<'a>>, Loc<TypeAnnotation<'a>>),
    // TODO in canonicalization, check to see if there are any newlines after the
    // annotation; if not, and if it's followed by a Body, then the annotation
    // applies to that expr! (TODO: verify that the pattern for both annotation and body match.)
    // No need to track that relationship in any data structure.
    Body(Loc<Pattern<'a>>, &'a Loc<Expr<'a>>),
    // TODO also in canonicalization, if there is a CustomType or TypeAlias
    // inside an Expr, give an error like "hey you need to move this to the
    // top level" - it'll parse fine, we just won't accept it there.
    CustomType(Loc<TypeAnnotation<'a>>, Vec<'a, Loc<TypeAnnotation<'a>>>),
    TypeAlias(Loc<TypeAnnotation<'a>>, Loc<TypeAnnotation<'a>>),

    // Blank Space (e.g. comments, spaces, newlines) before or after a def.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Def<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Def<'a>, &'a [CommentOrNewline<'a>]),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation<'a> {
    /// A function. The types of its arguments, then the type of its return value.
    Function(&'a [TypeAnnotation<'a>], &'a TypeAnnotation<'a>),

    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(&'a [&'a str], &'a str, &'a [Loc<TypeAnnotation<'a>>]),

    /// A bound type variable, e.g. `a` in `(a -> a)`
    BoundVariable(&'a str),

    /// A plain record, e.g. `{ name: String, email: Email }`
    Record(Vec<'a, Loc<AssignedField<'a, TypeAnnotation<'a>>>>),

    /// A record fragment, e.g. `{ name: String, email: Email }...r`
    RecordFragment(
        Vec<'a, Loc<AssignedField<'a, TypeAnnotation<'a>>>>,
        // the fragment type variable, e.g. the `r` in `{ name: String }...r`
        &'a Loc<TypeAnnotation<'a>>,
    ),

    /// The `*` type variable, e.g. in (List *)
    Wildcard,

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),

    /// A malformed type annotation, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignedField<'a, Val> {
    // Both a label and a value, e.g. `{ name: "blah" }`
    LabeledValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Val>),

    // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
    LabelOnly(Loc<&'a str>),

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a AssignedField<'a, Val>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a AssignedField<'a, Val>, &'a [CommentOrNewline<'a>]),

    /// A malformed assigned field, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Debug, PartialEq)]
pub enum CommentOrNewline<'a> {
    Newline,
    LineComment(&'a str),
}

impl<'a> CommentOrNewline<'a> {
    pub fn contains_newline(&self) -> bool {
        use self::CommentOrNewline::*;

        match self {
            // Line comments have an implicit newline at the end
            Newline | LineComment(_) => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'a> {
    // Identifier
    Identifier(&'a str),

    GlobalTag(&'a str),
    PrivateTag(&'a str),
    Apply(&'a Loc<Pattern<'a>>, &'a [Loc<Pattern<'a>>]),
    /// This is Loc<Pattern> rather than Loc<str> so we can record comments
    /// around the destructured names, e.g. { x ### x does stuff ###, y }
    /// In practice, these patterns will always be Identifier
    RecordDestructure(Vec<'a, Loc<Pattern<'a>>>),
    /// A field pattern, e.g. { x: Just 0 } -> ...
    /// can only occur inside of a RecordDestructure
    RecordField(&'a str, &'a Loc<Pattern<'a>>),

    /// This is used only to avoid cloning when reordering expressions (e.g. in desugar()).
    /// It lets us take an (&Expr) and create a plain (Expr) from it.
    Nested(&'a Pattern<'a>),

    // Literal
    IntLiteral(&'a str),
    HexIntLiteral(&'a str),
    OctalIntLiteral(&'a str),
    BinaryIntLiteral(&'a str),
    FloatLiteral(&'a str),
    StrLiteral(&'a str),
    BlockStrLiteral(&'a [&'a str]),
    EmptyRecordLiteral,
    Underscore,

    // Space
    SpaceBefore(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),

    // Malformed
    Malformed(&'a str),
    QualifiedIdentifier(MaybeQualified<'a, &'a str>),
}

impl<'a> Pattern<'a> {
    pub fn from_ident(arena: &'a Bump, ident: Ident<'a>) -> Pattern<'a> {
        match ident {
            Ident::GlobalTag(string) => Pattern::GlobalTag(string),
            Ident::PrivateTag(string) => Pattern::PrivateTag(string),
            Ident::Access(maybe_qualified) => {
                if maybe_qualified.value.len() == 1 {
                    Pattern::Identifier(maybe_qualified.value.iter().next().unwrap())
                } else {
                    let mut buf = String::with_capacity_in(
                        maybe_qualified.module_parts.len() + maybe_qualified.value.len(),
                        arena,
                    );

                    for part in maybe_qualified.module_parts.iter() {
                        buf.push_str(part);
                        buf.push('.');
                    }

                    let mut iter = maybe_qualified.value.iter().peekable();

                    while let Some(part) = iter.next() {
                        buf.push_str(part);

                        // If there are more fields to come, add a "."
                        if iter.peek().is_some() {
                            buf.push('.');
                        }
                    }

                    Pattern::Malformed(buf.into_bump_str())
                }
            }
            Ident::AccessorFunction(string) => Pattern::Malformed(string),
            Ident::Malformed(string) => Pattern::Malformed(string),
        }
    }
}

pub trait Spaceable<'a> {
    fn before(&'a self, _: &'a [CommentOrNewline<'a>]) -> Self;
    fn after(&'a self, _: &'a [CommentOrNewline<'a>]) -> Self;

    fn with_spaces_before(&'a self, spaces: &'a [CommentOrNewline<'a>], region: Region) -> Loc<Self>
    where
        Self: Sized,
    {
        Loc {
            region,
            value: self.before(spaces),
        }
    }

    fn with_spaces_after(&'a self, spaces: &'a [CommentOrNewline<'a>], region: Region) -> Loc<Self>
    where
        Self: Sized,
    {
        Loc {
            region,
            value: self.after(spaces),
        }
    }
}

impl<'a> Spaceable<'a> for Expr<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Expr::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Expr::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for Pattern<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Pattern::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Pattern::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for TypeAnnotation<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        TypeAnnotation::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        TypeAnnotation::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for ExposesEntry<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        ExposesEntry::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        ExposesEntry::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for ImportsEntry<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        ImportsEntry::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        ImportsEntry::SpaceAfter(self, spaces)
    }
}

impl<'a, Val> Spaceable<'a> for AssignedField<'a, Val> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        AssignedField::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        AssignedField::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for Def<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Def::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Def::SpaceAfter(self, spaces)
    }
}

/// What we're currently attempting to parse, e.g.
/// "currently attempting to parse a list." This helps error messages!
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Attempting {
    List,
    Keyword,
    StringLiteral,
    RecordLiteral,
    RecordFieldLabel,
    InterpolatedString,
    NumberLiteral,
    UnicodeEscape,
    ClosureParams,
    ClosureBody,
    Def,
    Module,
    Record,
    Identifier,
    ConcreteType,
    TypeVariable,
    CaseCondition,
    CaseBranch,
}

impl<'a> Expr<'a> {
    pub fn loc_ref(&'a self, region: Region) -> Loc<&'a Self> {
        Loc {
            region,
            value: self,
        }
    }

    pub fn loc(self, region: Region) -> Loc<Self> {
        Loc {
            region,
            value: self,
        }
    }
}
