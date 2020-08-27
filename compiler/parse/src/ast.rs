use crate::header::ModuleName;
use crate::ident::Ident;
use bumpalo::collections::String;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_module::operator::{BinOp, CalledVia, UnaryOp};
use roc_region::all::{Loc, Region};

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
pub struct WhenBranch<'a> {
    pub patterns: Vec<'a, Loc<Pattern<'a>>>,
    pub value: Loc<Expr<'a>>,
    pub guard: Option<Loc<Expr<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppHeader<'a> {
    pub name: Loc<ModuleName<'a>>,
    pub provides: Vec<'a, Loc<ExposesEntry<'a>>>,
    pub imports: Vec<'a, Loc<ImportsEntry<'a>>>,

    // Potential comments and newlines - these will typically all be empty.
    pub after_interface: &'a [CommentOrNewline<'a>],
    pub before_provides: &'a [CommentOrNewline<'a>],
    pub after_provides: &'a [CommentOrNewline<'a>],
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExposesEntry<'a> {
    /// e.g. `Task`
    Ident(&'a str),

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

impl<'a> ExposesEntry<'a> {
    pub fn as_str(&'a self) -> &'a str {
        use ExposesEntry::*;

        match self {
            Ident(string) => string,
            SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => sub_entry.as_str(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhenPattern<'a> {
    pub pattern: Loc<Pattern<'a>>,
    pub guard: Option<Loc<Expr<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StrSegment<'a> {
    Plaintext(&'a str),    // e.g. "foo"
    Unicode(Loc<&'a str>), // e.g. "00A0" in "\u(00A0)"
    EscapedChar(char),     // e.g. '\n' in "Hello!\n"
    Interpolated {
        // e.g. "App.version" in "Version: \(App.version)"
        module_name: &'a str,
        ident: &'a str,
        region: Region,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum StrLiteral<'a> {
    PlainLine(&'a str),
    LineWithEscapes(&'a [StrSegment<'a>]),
    Block(&'a [&'a [StrSegment<'a>]]),
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
    Num(&'a str),
    NonBase10Int {
        string: &'a str,
        base: Base,
        is_negative: bool,
    },

    // String Literals
    Str(StrLiteral<'a>), // string without escapes in it
    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access(&'a Expr<'a>, &'a str),
    /// e.g. `.foo`
    AccessorFunction(&'a str),

    // Collection Literals
    List(Vec<'a, &'a Loc<Expr<'a>>>),
    Record {
        update: Option<&'a Loc<Expr<'a>>>,
        fields: Vec<'a, Loc<AssignedField<'a, Expr<'a>>>>,
    },

    // Lookups
    Var {
        module_name: &'a str,
        ident: &'a str,
    },

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
    If(&'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>),
    When(
        /// The condition
        &'a Loc<Expr<'a>>,
        /// A | B if bool -> expression
        /// <Pattern 1> | <Pattern 2> if <Guard> -> <Expr>
        /// Vec, because there may be many patterns, and the guard
        /// is Option<Expr> because each branch may be preceded by
        /// a guard (".. if ..").
        Vec<'a, &'a WhenBranch<'a>>,
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
    PrecedenceConflict(Region, Loc<BinOp>, Loc<BinOp>, &'a Loc<Expr<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Def<'a> {
    // TODO in canonicalization, validate the pattern; only certain patterns
    // are allowed in annotations.
    Annotation(Loc<Pattern<'a>>, Loc<TypeAnnotation<'a>>),

    /// A type alias. This is like a standalone annotation, except the pattern
    /// must be a capitalized Identifier, e.g.
    ///
    /// Foo : Bar Baz
    Alias {
        name: Loc<&'a str>,
        vars: &'a [Loc<Pattern<'a>>],
        ann: Loc<TypeAnnotation<'a>>,
    },

    // TODO in canonicalization, check to see if there are any newlines after the
    // annotation; if not, and if it's followed by a Body, then the annotation
    // applies to that expr! (TODO: verify that the pattern for both annotation and body match.)
    // No need to track that relationship in any data structure.
    Body(&'a Loc<Pattern<'a>>, &'a Loc<Expr<'a>>),

    TypedBody(
        &'a Loc<Pattern<'a>>,
        Loc<TypeAnnotation<'a>>,
        &'a Loc<Expr<'a>>,
    ),

    // Blank Space (e.g. comments, spaces, newlines) before or after a def.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Def<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Def<'a>, &'a [CommentOrNewline<'a>]),

    /// This is used only to avoid cloning when reordering expressions (e.g. in desugar()).
    /// It lets us take a (&Def) and create a plain (Def) from it.
    Nested(&'a Def<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation<'a> {
    /// A function. The types of its arguments, then the type of its return value.
    Function(&'a [Loc<TypeAnnotation<'a>>], &'a Loc<TypeAnnotation<'a>>),

    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(&'a str, &'a str, &'a [Loc<TypeAnnotation<'a>>]),

    /// A bound type variable, e.g. `a` in `(a -> a)`
    BoundVariable(&'a str),

    /// Inline type alias, e.g. `as List a` in `[ Cons a (List a), Nil ] as List a`
    As(
        &'a Loc<TypeAnnotation<'a>>,
        &'a [CommentOrNewline<'a>],
        &'a Loc<TypeAnnotation<'a>>,
    ),

    Record {
        fields: &'a [Loc<AssignedField<'a, TypeAnnotation<'a>>>],
        /// The row type variable in an open record, e.g. the `r` in `{ name: Str }r`.
        /// This is None if it's a closed record annotation like `{ name: Str }`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
    },

    /// A tag union, e.g. `[
    TagUnion {
        tags: &'a [Loc<Tag<'a>>],
        /// The row type variable in an open tag union, e.g. the `a` in `[ Foo, Bar ]a`.
        /// This is None if it's a closed tag union like `[ Foo, Bar]`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
    },

    /// The `*` type variable, e.g. in (List *)
    Wildcard,

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),

    /// A malformed type annotation, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tag<'a> {
    Global {
        name: Loc<&'a str>,
        args: &'a [Loc<TypeAnnotation<'a>>],
    },

    Private {
        name: Loc<&'a str>,
        args: &'a [Loc<TypeAnnotation<'a>>],
    },

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Tag<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Tag<'a>, &'a [CommentOrNewline<'a>]),

    /// A malformed tag, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignedField<'a, Val> {
    // A required field with a label, e.g. `{ name: "blah" }` or `{ name : Str }`
    RequiredValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Val>),

    // An optional field with a label, e.g. `{ name ? "blah" }`
    //
    // NOTE: This only comes up in type annotations (e.g. `name ? Str`)
    // and in destructuring patterns (e.g. `{ name ? "blah" }`)
    OptionalValue(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Val>),

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
    DocComment(&'a str),
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
    RecordDestructure(&'a [Loc<Pattern<'a>>]),

    /// A required field pattern, e.g. { x: Just 0 } -> ...
    /// Can only occur inside of a RecordDestructure
    RequiredField(&'a str, &'a Loc<Pattern<'a>>),

    /// An optional field pattern, e.g. { x ? Just 0 } -> ...
    /// Can only occur inside of a RecordDestructure
    OptionalField(&'a str, &'a Loc<Expr<'a>>),

    /// This is used only to avoid cloning when reordering expressions (e.g. in desugar()).
    /// It lets us take an (&Expr) and create a plain (Expr) from it.
    Nested(&'a Pattern<'a>),

    // Literal
    NumLiteral(&'a str),
    NonBase10Literal {
        string: &'a str,
        base: Base,
        is_negative: bool,
    },
    FloatLiteral(&'a str),
    StrLiteral(StrLiteral<'a>),
    Underscore,

    // Space
    SpaceBefore(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),

    // Malformed
    Malformed(&'a str),
    QualifiedIdentifier {
        module_name: &'a str,
        ident: &'a str,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Base {
    Octal,
    Binary,
    Hex,
    Decimal,
}

impl<'a> Pattern<'a> {
    pub fn from_ident(arena: &'a Bump, ident: Ident<'a>) -> Pattern<'a> {
        match ident {
            Ident::GlobalTag(string) => Pattern::GlobalTag(string),
            Ident::PrivateTag(string) => Pattern::PrivateTag(string),
            Ident::Access { module_name, parts } => {
                if parts.len() == 1 {
                    // This is valid iff there is no module.
                    let ident = parts.iter().next().unwrap();

                    if module_name.is_empty() {
                        Pattern::Identifier(ident)
                    } else {
                        Pattern::QualifiedIdentifier { module_name, ident }
                    }
                } else {
                    // This is definitely malformed.
                    let mut buf =
                        String::with_capacity_in(module_name.len() + (2 * parts.len()), arena);
                    let mut any_parts_printed = if module_name.is_empty() {
                        false
                    } else {
                        buf.push_str(module_name);

                        true
                    };

                    for part in parts.iter() {
                        if any_parts_printed {
                            buf.push('.');
                        } else {
                            any_parts_printed = true;
                        }

                        buf.push_str(part);
                    }

                    Pattern::Malformed(buf.into_bump_str())
                }
            }
            Ident::AccessorFunction(string) => Pattern::Malformed(string),
            Ident::Malformed(string) => Pattern::Malformed(string),
        }
    }

    /// Check that patterns are equivalent, meaning they have the same shape, but may have
    /// different locations/whitespace
    pub fn equivalent(&self, other: &Self) -> bool {
        use Pattern::*;

        match (self, other) {
            (Identifier(x), Identifier(y)) => x == y,
            (GlobalTag(x), GlobalTag(y)) => x == y,
            (PrivateTag(x), PrivateTag(y)) => x == y,
            (Apply(constructor_x, args_x), Apply(constructor_y, args_y)) => {
                let equivalent_args = args_x
                    .iter()
                    .zip(args_y.iter())
                    .all(|(p, q)| p.value.equivalent(&q.value));

                constructor_x.value.equivalent(&constructor_y.value) && equivalent_args
            }
            (RecordDestructure(fields_x), RecordDestructure(fields_y)) => fields_x
                .iter()
                .zip(fields_y.iter())
                .all(|(p, q)| p.value.equivalent(&q.value)),
            (RequiredField(x, inner_x), RequiredField(y, inner_y)) => {
                x == y && inner_x.value.equivalent(&inner_y.value)
            }
            (OptionalField(x, _), OptionalField(y, _))
            | (OptionalField(x, _), Identifier(y))
            | (Identifier(x), OptionalField(y, _)) => {
                // optional record fields can be annotated as:
                //      { x, y } : { x : Int, y ? Bool }
                //      { x, y ? False } = rec
                x == y
            }
            (Nested(x), Nested(y)) => x.equivalent(y),

            // Literal
            (NumLiteral(x), NumLiteral(y)) => x == y,
            (
                NonBase10Literal {
                    string: string_x,
                    base: base_x,
                    is_negative: is_negative_x,
                },
                NonBase10Literal {
                    string: string_y,
                    base: base_y,
                    is_negative: is_negative_y,
                },
            ) => string_x == string_y && base_x == base_y && is_negative_x == is_negative_y,
            (FloatLiteral(x), FloatLiteral(y)) => x == y,
            (StrLiteral(x), StrLiteral(y)) => x == y,
            (Underscore, Underscore) => true,

            // Space
            (SpaceBefore(x, _), SpaceBefore(y, _)) => x.equivalent(y),
            (SpaceAfter(x, _), SpaceAfter(y, _)) => x.equivalent(y),

            // Malformed
            (Malformed(x), Malformed(y)) => x == y,
            (
                QualifiedIdentifier {
                    module_name: a,
                    ident: x,
                },
                QualifiedIdentifier {
                    module_name: b,
                    ident: y,
                },
            ) => (a == b) && (x == y),

            // Different constructors
            _ => false,
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

impl<'a> Spaceable<'a> for Tag<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Tag::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Tag::SpaceAfter(self, spaces)
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
    StrLiteral,
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
    WhenCondition,
    WhenBranch,
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
