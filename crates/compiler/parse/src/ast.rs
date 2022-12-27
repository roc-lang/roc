use std::fmt::Debug;

use crate::header::{AppHeader, HostedHeader, InterfaceHeader, PackageHeader, PlatformHeader};
use crate::ident::Ident;
use bumpalo::collections::{String, Vec};
use bumpalo::Bump;
use roc_collections::soa::{EitherIndex, Index, Slice};
use roc_module::called_via::{BinOp, CalledVia, UnaryOp};
use roc_region::all::{Loc, Position, Region};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spaces<'a, T> {
    pub before: &'a [CommentOrNewline<'a>],
    pub item: T,
    pub after: &'a [CommentOrNewline<'a>],
}

#[derive(Copy, Clone, PartialEq)]
pub enum Spaced<'a, T> {
    Item(T),

    // Spaces
    SpaceBefore(&'a Spaced<'a, T>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Spaced<'a, T>, &'a [CommentOrNewline<'a>]),
}

impl<'a, T> Spaced<'a, T> {
    /// A `Spaced` is multiline if it has newlines or comments before or after the item, since
    /// comments induce newlines!
    pub fn is_multiline(&self) -> bool {
        match self {
            Spaced::Item(_) => false,
            Spaced::SpaceBefore(_, spaces) | Spaced::SpaceAfter(_, spaces) => {
                debug_assert!(!spaces.is_empty());
                true
            }
        }
    }
}

impl<'a, T: Debug> Debug for Spaced<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Item(item) => item.fmt(f),
            Self::SpaceBefore(item, space) => f
                .debug_tuple("SpaceBefore")
                .field(item)
                .field(space)
                .finish(),
            Self::SpaceAfter(item, space) => f
                .debug_tuple("SpaceAfter")
                .field(item)
                .field(space)
                .finish(),
        }
    }
}

pub trait ExtractSpaces<'a>: Sized + Copy {
    type Item;
    fn extract_spaces(&self) -> Spaces<'a, Self::Item>;
}

impl<'a, T: ExtractSpaces<'a>> ExtractSpaces<'a> for &'a T {
    type Item = T::Item;
    fn extract_spaces(&self) -> Spaces<'a, Self::Item> {
        (*self).extract_spaces()
    }
}

impl<'a, T: ExtractSpaces<'a>> ExtractSpaces<'a> for Loc<T> {
    type Item = T::Item;
    fn extract_spaces(&self) -> Spaces<'a, Self::Item> {
        let spaces = self.value.extract_spaces();
        Spaces {
            before: spaces.before,
            item: spaces.item,
            after: spaces.after,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module<'a> {
    pub comments: &'a [CommentOrNewline<'a>],
    pub header: Header<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Header<'a> {
    Interface(InterfaceHeader<'a>),
    App(AppHeader<'a>),
    Package(PackageHeader<'a>),
    Platform(PlatformHeader<'a>),
    Hosted(HostedHeader<'a>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WhenBranch<'a> {
    pub patterns: &'a [Loc<Pattern<'a>>],
    pub value: Loc<Expr<'a>>,
    pub guard: Option<Loc<Expr<'a>>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WhenPattern<'a> {
    pub pattern: Loc<Pattern<'a>>,
    pub guard: Option<Loc<Expr<'a>>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StrSegment<'a> {
    Plaintext(&'a str),              // e.g. "foo"
    Unicode(Loc<&'a str>),           // e.g. "00A0" in "\u(00A0)"
    EscapedChar(EscapedChar),        // e.g. '\n' in "Hello!\n"
    Interpolated(Loc<&'a Expr<'a>>), // e.g. (name) in "Hi, \(name)!"
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EscapedChar {
    Newline,        // \n
    Tab,            // \t
    Quote,          // \"
    Backslash,      // \\
    CarriageReturn, // \r
}

impl EscapedChar {
    /// Returns the char that would have been originally parsed to
    pub fn to_parsed_char(self) -> char {
        use EscapedChar::*;

        match self {
            Backslash => '\\',
            Quote => '"',
            CarriageReturn => 'r',
            Tab => 't',
            Newline => 'n',
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StrLiteral<'a> {
    /// The most common case: a plain string with no escapes or interpolations
    PlainLine(&'a str),
    Line(&'a [StrSegment<'a>]),
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
#[derive(Clone, Copy, Debug, PartialEq)]
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
    /// eg 'b'
    SingleQuote(&'a str),

    /// Look up exactly one field on a record, e.g. `x.foo`.
    RecordAccess(&'a Expr<'a>, &'a str),
    /// e.g. `.foo`
    RecordAccessorFunction(&'a str),

    /// Look up exactly one field on a tuple, e.g. `(x, y).1`.
    TupleAccess(&'a Expr<'a>, &'a str),
    /// e.g. `.1`
    TupleAccessorFunction(&'a str),

    // Collection Literals
    List(Collection<'a, &'a Loc<Expr<'a>>>),

    RecordUpdate {
        update: &'a Loc<Expr<'a>>,
        fields: Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>,
    },

    Record(Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    Tuple(Collection<'a, &'a Loc<Expr<'a>>>),

    // Lookups
    Var {
        module_name: &'a str, // module_name will only be filled if the original Roc code stated something like `5 + SomeModule.myVar`, module_name will be blank if it was `5 + myVar`
        ident: &'a str,
    },

    Underscore(&'a str),

    // The "crash" keyword
    Crash,

    // Tags
    Tag(&'a str),

    // Reference to an opaque type, e.g. @Opaq
    OpaqueRef(&'a str),

    // Pattern Matching
    Closure(&'a [Loc<Pattern<'a>>], &'a Loc<Expr<'a>>),
    /// Multiple defs in a row
    Defs(&'a Defs<'a>, &'a Loc<Expr<'a>>),
    Backpassing(&'a [Loc<Pattern<'a>>], &'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>),
    Expect(&'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>),
    Dbg(&'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>),

    // Application
    /// To apply by name, do Apply(Var(...), ...)
    /// To apply a tag by name, do Apply(Tag(...), ...)
    Apply(&'a Loc<Expr<'a>>, &'a [&'a Loc<Expr<'a>>], CalledVia),
    BinOps(&'a [(Loc<Expr<'a>>, Loc<BinOp>)], &'a Loc<Expr<'a>>),
    UnaryOp(&'a Loc<Expr<'a>>, Loc<UnaryOp>),

    // Conditionals
    If(&'a [(Loc<Expr<'a>>, Loc<Expr<'a>>)], &'a Loc<Expr<'a>>),
    When(
        /// The condition
        &'a Loc<Expr<'a>>,
        /// A | B if bool -> expression
        /// <Pattern 1> | <Pattern 2> if <Guard> -> <Expr>
        /// Vec, because there may be many patterns, and the guard
        /// is Option<Expr> because each branch may be preceded by
        /// a guard (".. if ..").
        &'a [&'a WhenBranch<'a>],
    ),

    // Blank Space (e.g. comments, spaces, newlines) before or after an expression.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    ParensAround(&'a Expr<'a>),

    // Problems
    MalformedIdent(&'a str, crate::ident::BadIdent),
    MalformedClosure,
    // Both operators were non-associative, e.g. (True == False == False).
    // We should tell the author to disambiguate by grouping them with parens.
    PrecedenceConflict(&'a PrecedenceConflict<'a>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PrecedenceConflict<'a> {
    pub whole_region: Region,
    pub binop1_position: Position,
    pub binop2_position: Position,
    pub binop1: BinOp,
    pub binop2: BinOp,
    pub expr: &'a Loc<Expr<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeHeader<'a> {
    pub name: Loc<&'a str>,
    pub vars: &'a [Loc<Pattern<'a>>],
}

impl<'a> TypeHeader<'a> {
    pub fn region(&self) -> Region {
        Region::across_all(
            [self.name.region]
                .iter()
                .chain(self.vars.iter().map(|v| &v.region)),
        )
    }
}

/// The `has` keyword associated with ability definitions.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Has<'a> {
    Has,
    SpaceBefore(&'a Has<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Has<'a>, &'a [CommentOrNewline<'a>]),
}

/// An ability demand is a value defining the ability; for example `hash : a -> U64 | a has Hash`
/// for a `Hash` ability.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AbilityMember<'a> {
    pub name: Loc<Spaced<'a, &'a str>>,
    pub typ: Loc<TypeAnnotation<'a>>,
}

impl AbilityMember<'_> {
    pub fn region(&self) -> Region {
        Region::across_all([self.name.region, self.typ.region].iter())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeDef<'a> {
    /// A type alias. This is like a standalone annotation, except the pattern
    /// must be a capitalized Identifier, e.g.
    ///
    /// Foo : Bar Baz
    Alias {
        header: TypeHeader<'a>,
        ann: Loc<TypeAnnotation<'a>>,
    },

    /// An opaque type, wrapping its inner type. E.g. Age := U64.
    Opaque {
        header: TypeHeader<'a>,
        typ: Loc<TypeAnnotation<'a>>,
        derived: Option<Loc<HasAbilities<'a>>>,
    },

    /// An ability definition. E.g.
    ///   Hash has
    ///     hash : a -> U64 | a has Hash
    Ability {
        header: TypeHeader<'a>,
        loc_has: Loc<Has<'a>>,
        members: &'a [AbilityMember<'a>],
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueDef<'a> {
    // TODO in canonicalization, validate the pattern; only certain patterns
    // are allowed in annotations.
    Annotation(Loc<Pattern<'a>>, Loc<TypeAnnotation<'a>>),

    // TODO in canonicalization, check to see if there are any newlines after the
    // annotation; if not, and if it's followed by a Body, then the annotation
    // applies to that expr! (TODO: verify that the pattern for both annotation and body match.)
    // No need to track that relationship in any data structure.
    Body(&'a Loc<Pattern<'a>>, &'a Loc<Expr<'a>>),

    AnnotatedBody {
        ann_pattern: &'a Loc<Pattern<'a>>,
        ann_type: &'a Loc<TypeAnnotation<'a>>,
        comment: Option<&'a str>,
        body_pattern: &'a Loc<Pattern<'a>>,
        body_expr: &'a Loc<Expr<'a>>,
    },

    Dbg {
        condition: &'a Loc<Expr<'a>>,
        preceding_comment: Region,
    },

    Expect {
        condition: &'a Loc<Expr<'a>>,
        preceding_comment: Region,
    },

    ExpectFx {
        condition: &'a Loc<Expr<'a>>,
        preceding_comment: Region,
    },
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Defs<'a> {
    pub tags: std::vec::Vec<EitherIndex<TypeDef<'a>, ValueDef<'a>>>,
    pub regions: std::vec::Vec<Region>,
    pub space_before: std::vec::Vec<Slice<CommentOrNewline<'a>>>,
    pub space_after: std::vec::Vec<Slice<CommentOrNewline<'a>>>,
    pub spaces: std::vec::Vec<CommentOrNewline<'a>>,
    pub type_defs: std::vec::Vec<TypeDef<'a>>,
    pub value_defs: std::vec::Vec<ValueDef<'a>>,
}

impl<'a> Defs<'a> {
    pub fn is_empty(&self) -> bool {
        self.tags.is_empty()
    }

    pub fn len(&self) -> usize {
        self.tags.len()
    }

    pub fn defs(&self) -> impl Iterator<Item = Result<&TypeDef<'a>, &ValueDef<'a>>> {
        self.tags.iter().map(|tag| match tag.split() {
            Ok(type_index) => Ok(&self.type_defs[type_index.index()]),
            Err(value_index) => Err(&self.value_defs[value_index.index()]),
        })
    }

    pub fn last(&self) -> Option<Result<&TypeDef<'a>, &ValueDef<'a>>> {
        self.tags.last().map(|tag| match tag.split() {
            Ok(type_index) => Ok(&self.type_defs[type_index.index()]),
            Err(value_index) => Err(&self.value_defs[value_index.index()]),
        })
    }

    /// NOTE assumes the def itself is pushed already!
    fn push_def_help(
        &mut self,
        tag: EitherIndex<TypeDef<'a>, ValueDef<'a>>,
        region: Region,
        spaces_before: &[CommentOrNewline<'a>],
        spaces_after: &[CommentOrNewline<'a>],
    ) {
        self.tags.push(tag);

        self.regions.push(region);

        let before = Slice::extend_new(&mut self.spaces, spaces_before.iter().copied());
        self.space_before.push(before);

        let after = Slice::extend_new(&mut self.spaces, spaces_after.iter().copied());
        self.space_after.push(after);
    }

    pub fn push_value_def(
        &mut self,
        value_def: ValueDef<'a>,
        region: Region,
        spaces_before: &[CommentOrNewline<'a>],
        spaces_after: &[CommentOrNewline<'a>],
    ) {
        let value_def_index = Index::push_new(&mut self.value_defs, value_def);
        let tag = EitherIndex::from_right(value_def_index);
        self.push_def_help(tag, region, spaces_before, spaces_after)
    }

    pub fn replace_with_value_def(
        &mut self,
        index: usize,
        value_def: ValueDef<'a>,
        region: Region,
    ) {
        let value_def_index = Index::push_new(&mut self.value_defs, value_def);
        let tag = EitherIndex::from_right(value_def_index);

        self.tags[index] = tag;
        self.regions[index] = region;
    }

    pub fn push_type_def(
        &mut self,
        type_def: TypeDef<'a>,
        region: Region,
        spaces_before: &[CommentOrNewline<'a>],
        spaces_after: &[CommentOrNewline<'a>],
    ) {
        let type_def_index = Index::push_new(&mut self.type_defs, type_def);
        let tag = EitherIndex::from_left(type_def_index);
        self.push_def_help(tag, region, spaces_before, spaces_after)
    }
}

/// Should always be a zero-argument `Apply`; we'll check this in canonicalization
pub type AbilityName<'a> = Loc<TypeAnnotation<'a>>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct HasClause<'a> {
    pub var: Loc<Spaced<'a, &'a str>>,
    pub abilities: &'a [AbilityName<'a>],
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum HasImpls<'a> {
    // `{ eq: myEq }`
    HasImpls(Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a HasImpls<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a HasImpls<'a>, &'a [CommentOrNewline<'a>]),
}

/// `Eq` or `Eq { eq: myEq }`
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum HasAbility<'a> {
    HasAbility {
        /// Should be a zero-argument `Apply` or an error; we'll check this in canonicalization
        ability: Loc<TypeAnnotation<'a>>,
        impls: Option<Loc<HasImpls<'a>>>,
    },

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a HasAbility<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a HasAbility<'a>, &'a [CommentOrNewline<'a>]),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum HasAbilities<'a> {
    /// `has [Eq { eq: myEq }, Hash]`
    Has(Collection<'a, Loc<HasAbility<'a>>>),

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a HasAbilities<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a HasAbilities<'a>, &'a [CommentOrNewline<'a>]),
}

impl HasAbilities<'_> {
    pub fn collection(&self) -> &Collection<Loc<HasAbility>> {
        let mut it = self;
        loop {
            match it {
                Self::SpaceBefore(inner, _) | Self::SpaceAfter(inner, _) => {
                    it = inner;
                }
                Self::Has(collection) => return collection,
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.collection().is_empty()
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeAnnotation<'a> {
    /// A function. The types of its arguments, then the type of its return value.
    Function(&'a [Loc<TypeAnnotation<'a>>], &'a Loc<TypeAnnotation<'a>>),

    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(&'a str, &'a str, &'a [Loc<TypeAnnotation<'a>>]),

    /// A bound type variable, e.g. `a` in `(a -> a)`
    BoundVariable(&'a str),

    /// Inline type alias, e.g. `as List a` in `[Cons a (List a), Nil] as List a`
    As(
        &'a Loc<TypeAnnotation<'a>>,
        &'a [CommentOrNewline<'a>],
        TypeHeader<'a>,
    ),

    Record {
        fields: Collection<'a, Loc<AssignedField<'a, TypeAnnotation<'a>>>>,
        /// The row type variable in an open record, e.g. the `r` in `{ name: Str }r`.
        /// This is None if it's a closed record annotation like `{ name: Str }`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
    },

    Tuple {
        fields: Collection<'a, Loc<TypeAnnotation<'a>>>,
        /// The row type variable in an open tuple, e.g. the `r` in `( Str, Str )r`.
        /// This is None if it's a closed tuple annotation like `( Str, Str )`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
    },

    /// A tag union, e.g. `[
    TagUnion {
        /// The row type variable in an open tag union, e.g. the `a` in `[Foo, Bar]a`.
        /// This is None if it's a closed tag union like `[Foo, Bar]`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
        tags: Collection<'a, Loc<Tag<'a>>>,
    },

    /// '_', indicating the compiler should infer the type
    Inferred,

    /// The `*` type variable, e.g. in (List *)
    Wildcard,

    /// A "where" clause demanding abilities designated by a `|`, e.g. `a -> U64 | a has Hash`
    Where(&'a Loc<TypeAnnotation<'a>>, &'a [Loc<HasClause<'a>>]),

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),

    /// A malformed type annotation, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Tag<'a> {
    Apply {
        name: Loc<&'a str>,
        args: &'a [Loc<TypeAnnotation<'a>>],
    },

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Tag<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Tag<'a>, &'a [CommentOrNewline<'a>]),

    /// A malformed tag, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommentOrNewline<'a> {
    Newline,
    LineComment(&'a str),
    DocComment(&'a str),
}

impl<'a> CommentOrNewline<'a> {
    pub fn is_comment(&self) -> bool {
        use CommentOrNewline::*;
        match self {
            Newline => false,
            LineComment(_) => true,
            DocComment(_) => true,
        }
    }

    pub fn is_newline(&self) -> bool {
        use CommentOrNewline::*;
        match self {
            Newline => true,
            LineComment(_) => false,
            DocComment(_) => false,
        }
    }

    pub fn to_string_repr(&self) -> std::string::String {
        use CommentOrNewline::*;
        match self {
            Newline => "\n".to_owned(),
            LineComment(comment_str) => format!("#{}", comment_str),
            DocComment(comment_str) => format!("##{}", comment_str),
        }
    }

    pub fn comment_str(&'a self) -> Option<&'a str> {
        match self {
            CommentOrNewline::LineComment(s) => Some(*s),
            CommentOrNewline::DocComment(s) => Some(*s),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Pattern<'a> {
    // Identifier
    Identifier(&'a str),

    Tag(&'a str),

    OpaqueRef(&'a str),

    Apply(&'a Loc<Pattern<'a>>, &'a [Loc<Pattern<'a>>]),

    /// This is Located<Pattern> rather than Located<str> so we can record comments
    /// around the destructured names, e.g. { x ### x does stuff ###, y }
    /// In practice, these patterns will always be Identifier
    RecordDestructure(Collection<'a, Loc<Pattern<'a>>>),

    /// A required field pattern, e.g. { x: Just 0 } -> ...
    /// Can only occur inside of a RecordDestructure
    RequiredField(&'a str, &'a Loc<Pattern<'a>>),

    /// An optional field pattern, e.g. { x ? Just 0 } -> ...
    /// Can only occur inside of a RecordDestructure
    OptionalField(&'a str, &'a Loc<Expr<'a>>),

    // Literal
    NumLiteral(&'a str),
    NonBase10Literal {
        string: &'a str,
        base: Base,
        is_negative: bool,
    },
    FloatLiteral(&'a str),
    StrLiteral(StrLiteral<'a>),
    Underscore(&'a str),
    SingleQuote(&'a str),

    /// A tuple pattern, e.g. (Just x, 1)
    Tuple(Collection<'a, Loc<Pattern<'a>>>),

    /// A list pattern like [_, x, ..]
    List(Collection<'a, Loc<Pattern<'a>>>),

    /// A list-rest pattern ".."
    /// Can only occur inside of a [Pattern::List]
    ListRest,

    // Space
    SpaceBefore(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),

    // Malformed
    Malformed(&'a str),
    MalformedIdent(&'a str, crate::ident::BadIdent),
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
            Ident::Tag(string) => Pattern::Tag(string),
            Ident::OpaqueRef(string) => Pattern::OpaqueRef(string),
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
            Ident::RecordAccessorFunction(string) => Pattern::Malformed(string),
            Ident::TupleAccessorFunction(string) => Pattern::Malformed(string),
            Ident::Malformed(string, _problem) => Pattern::Malformed(string),
        }
    }

    /// Check that patterns are equivalent, meaning they have the same shape, but may have
    /// different locations/whitespace
    pub fn equivalent(&self, other: &Self) -> bool {
        use Pattern::*;

        match other {
            SpaceBefore(y, _) | SpaceAfter(y, _) => {
                return self.equivalent(y);
            }
            _ => {}
        }

        match self {
            Tag(x) => {
                if let Tag(y) = other {
                    x == y
                } else {
                    false
                }
            }
            Apply(constructor_x, args_x) => {
                if let Apply(constructor_y, args_y) = other {
                    let equivalent_args = args_x
                        .iter()
                        .zip(args_y.iter())
                        .all(|(p, q)| p.value.equivalent(&q.value));

                    constructor_x.value.equivalent(&constructor_y.value) && equivalent_args
                } else {
                    false
                }
            }
            RecordDestructure(fields_x) => {
                if let RecordDestructure(fields_y) = other {
                    fields_x
                        .iter()
                        .zip(fields_y.iter())
                        .all(|(p, q)| p.value.equivalent(&q.value))
                } else {
                    false
                }
            }
            RequiredField(x, inner_x) => {
                if let RequiredField(y, inner_y) = other {
                    x == y && inner_x.value.equivalent(&inner_y.value)
                } else {
                    false
                }
            }

            // optional record fields can be annotated as:
            //      { x, y } : { x : Int, y ? Bool }
            //      { x, y ? False } = rec
            OptionalField(x, _) => match other {
                Identifier(y) | OptionalField(y, _) => x == y,
                _ => false,
            },
            Identifier(x) => match other {
                Identifier(y) | OptionalField(y, _) => x == y,
                _ => false,
            },
            NumLiteral(x) => {
                if let NumLiteral(y) = other {
                    x == y
                } else {
                    false
                }
            }
            NonBase10Literal {
                string: string_x,
                base: base_x,
                is_negative: is_negative_x,
            } => {
                if let NonBase10Literal {
                    string: string_y,
                    base: base_y,
                    is_negative: is_negative_y,
                } = other
                {
                    string_x == string_y && base_x == base_y && is_negative_x == is_negative_y
                } else {
                    false
                }
            }
            FloatLiteral(x) => {
                if let FloatLiteral(y) = other {
                    x == y
                } else {
                    false
                }
            }
            StrLiteral(x) => {
                if let StrLiteral(y) = other {
                    x == y
                } else {
                    false
                }
            }
            Underscore(x) => {
                if let Underscore(y) = other {
                    x == y
                } else {
                    false
                }
            }
            SpaceBefore(x, _) | SpaceAfter(x, _) => match other {
                SpaceBefore(y, _) | SpaceAfter(y, _) => x.equivalent(y),
                y => x.equivalent(y),
            },
            Malformed(x) => {
                if let Malformed(y) = other {
                    x == y
                } else {
                    false
                }
            }
            QualifiedIdentifier {
                module_name: a,
                ident: x,
            } => {
                if let QualifiedIdentifier {
                    module_name: b,
                    ident: y,
                } = other
                {
                    a == b && x == y
                } else {
                    false
                }
            }
            OpaqueRef(a) => {
                if let OpaqueRef(b) = other {
                    a == b
                } else {
                    false
                }
            }
            SingleQuote(a) => {
                if let SingleQuote(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Tuple(items_x) => {
                if let Tuple(items_y) = other {
                    items_x
                        .iter()
                        .zip(items_y.iter())
                        .all(|(p, q)| p.value.equivalent(&q.value))
                } else {
                    false
                }
            }
            List(items_x) => {
                if let List(items_y) = other {
                    items_x
                        .iter()
                        .zip(items_y.iter())
                        .all(|(p, q)| p.value.equivalent(&q.value))
                } else {
                    false
                }
            }
            ListRest => matches!(other, ListRest),
            MalformedIdent(str_x, _) => {
                if let MalformedIdent(str_y, _) = other {
                    str_x == str_y
                } else {
                    false
                }
            }
        }
    }
}
#[derive(Copy, Clone)]
pub struct Collection<'a, T> {
    pub items: &'a [T],
    // Use a pointer to a slice (rather than just a slice), in order to avoid bloating
    // Ast variants. The final_comments field is rarely accessed in the hot path, so
    // this shouldn't matter much for perf.
    // Use an Option, so it's possible to initialize without allocating.
    final_comments: Option<&'a &'a [CommentOrNewline<'a>]>,
}

impl<'a, T> Collection<'a, T> {
    pub fn empty() -> Collection<'a, T> {
        Collection {
            items: &[],
            final_comments: None,
        }
    }

    pub const fn with_items(items: &'a [T]) -> Collection<'a, T> {
        Collection {
            items,
            final_comments: None,
        }
    }

    pub fn with_items_and_comments(
        arena: &'a Bump,
        items: &'a [T],
        comments: &'a [CommentOrNewline<'a>],
    ) -> Collection<'a, T> {
        Collection {
            items,
            final_comments: if comments.is_empty() {
                None
            } else {
                Some(arena.alloc(comments))
            },
        }
    }

    pub fn replace_items<V>(&self, new_items: &'a [V]) -> Collection<'a, V> {
        Collection {
            items: new_items,
            final_comments: self.final_comments,
        }
    }

    pub fn ptrify_items(&self, arena: &'a Bump) -> Collection<'a, &'a T> {
        let mut allocated = Vec::with_capacity_in(self.len(), arena);

        for parsed_elem in self.items {
            allocated.push(parsed_elem);
        }

        self.replace_items(allocated.into_bump_slice())
    }

    pub fn map_items<V: 'a>(&self, arena: &'a Bump, f: impl Fn(&'a T) -> V) -> Collection<'a, V> {
        let mut allocated = Vec::with_capacity_in(self.len(), arena);

        for parsed_elem in self.items {
            allocated.push(f(parsed_elem));
        }

        self.replace_items(allocated.into_bump_slice())
    }

    pub fn map_items_result<V: 'a, E>(
        &self,
        arena: &'a Bump,
        f: impl Fn(&T) -> Result<V, E>,
    ) -> Result<Collection<'a, V>, E> {
        let mut allocated = Vec::with_capacity_in(self.len(), arena);

        for parsed_elem in self.items {
            allocated.push(f(parsed_elem)?);
        }

        Ok(self.replace_items(allocated.into_bump_slice()))
    }

    pub fn final_comments(&self) -> &'a [CommentOrNewline<'a>] {
        if let Some(final_comments) = self.final_comments {
            final_comments
        } else {
            &[]
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &'a T> {
        self.items.iter()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl<'a, T: PartialEq> PartialEq for Collection<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items && self.final_comments() == other.final_comments()
    }
}

impl<'a, T: Debug> Debug for Collection<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.final_comments().is_empty() {
            f.debug_list().entries(self.items.iter()).finish()
        } else {
            f.debug_struct("Collection")
                .field("items", &self.items)
                .field("final_comments", &self.final_comments())
                .finish()
        }
    }
}

impl<'a, T> Default for Collection<'a, T> {
    fn default() -> Self {
        Self::empty()
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

impl<'a, T> Spaceable<'a> for Spaced<'a, T> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Spaced::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Spaced::SpaceAfter(self, spaces)
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

impl<'a> Spaceable<'a> for Has<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Has::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Has::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for HasImpls<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        HasImpls::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        HasImpls::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for HasAbility<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        HasAbility::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        HasAbility::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for HasAbilities<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        HasAbilities::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        HasAbilities::SpaceAfter(self, spaces)
    }
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

    pub fn is_tag(&self) -> bool {
        matches!(self, Expr::Tag(_))
    }

    pub fn is_opaque(&self) -> bool {
        matches!(self, Expr::OpaqueRef(_))
    }
}

macro_rules! impl_extract_spaces {
    ($t:ident $(< $($generic_args:ident),* >)?) => {

        impl<'a, $($($generic_args: Copy),*)?> ExtractSpaces<'a> for $t<'a, $($($generic_args),*)?> {
            type Item = Self;
            fn extract_spaces(&self) -> Spaces<'a, Self::Item> {
                match self {
                    $t::SpaceBefore(item, before) => {
                        match item {
                            $t::SpaceBefore(_, _) => todo!(),
                            $t::SpaceAfter(item, after) => {
                                Spaces {
                                    before,
                                    item: **item,
                                    after,
                                }
                            }
                            _ => {
                                Spaces {
                                    before,
                                    item: **item,
                                    after: &[],
                                }
                            }
                        }
                    },
                    $t::SpaceAfter(item, after) => {
                        match item {
                            $t::SpaceBefore(item, before) => {
                                Spaces {
                                    before,
                                    item: **item,
                                    after,
                                }
                            }
                            $t::SpaceAfter(_, _) => todo!(),
                            _ => {
                                Spaces {
                                    before: &[],
                                    item: **item,
                                    after,
                                }
                            }
                        }
                    },
                    _ => {
                        Spaces {
                            before: &[],
                            item: *self,
                            after: &[],
                        }
                    }
                }
            }
        }
    };
}

impl_extract_spaces!(Expr);
impl_extract_spaces!(Pattern);
impl_extract_spaces!(Tag);
impl_extract_spaces!(AssignedField<T>);
impl_extract_spaces!(TypeAnnotation);
impl_extract_spaces!(HasAbility);

impl<'a, T: Copy> ExtractSpaces<'a> for Spaced<'a, T> {
    type Item = T;

    fn extract_spaces(&self) -> Spaces<'a, T> {
        match self {
            Spaced::SpaceBefore(item, before) => match item {
                Spaced::SpaceBefore(_, _) => todo!(),
                Spaced::SpaceAfter(item, after) => {
                    if let Spaced::Item(item) = item {
                        Spaces {
                            before,
                            item: *item,
                            after,
                        }
                    } else {
                        todo!();
                    }
                }
                Spaced::Item(item) => Spaces {
                    before,
                    item: *item,
                    after: &[],
                },
            },
            Spaced::SpaceAfter(item, after) => match item {
                Spaced::SpaceBefore(item, before) => {
                    if let Spaced::Item(item) = item {
                        Spaces {
                            before,
                            item: *item,
                            after,
                        }
                    } else {
                        todo!();
                    }
                }
                Spaced::SpaceAfter(_, _) => todo!(),
                Spaced::Item(item) => Spaces {
                    before: &[],
                    item: *item,
                    after,
                },
            },
            Spaced::Item(item) => Spaces {
                before: &[],
                item: *item,
                after: &[],
            },
        }
    }
}

impl<'a> ExtractSpaces<'a> for HasImpls<'a> {
    type Item = Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>;

    fn extract_spaces(&self) -> Spaces<'a, Self::Item> {
        match self {
            HasImpls::HasImpls(inner) => Spaces {
                before: &[],
                item: *inner,
                after: &[],
            },
            HasImpls::SpaceBefore(item, before) => match item {
                HasImpls::HasImpls(inner) => Spaces {
                    before,
                    item: *inner,
                    after: &[],
                },
                HasImpls::SpaceBefore(_, _) => todo!(),
                HasImpls::SpaceAfter(HasImpls::HasImpls(inner), after) => Spaces {
                    before,
                    item: *inner,
                    after,
                },
                HasImpls::SpaceAfter(_, _) => todo!(),
            },
            HasImpls::SpaceAfter(item, after) => match item {
                HasImpls::HasImpls(inner) => Spaces {
                    before: &[],
                    item: *inner,
                    after,
                },
                HasImpls::SpaceBefore(HasImpls::HasImpls(inner), before) => Spaces {
                    before,
                    item: *inner,
                    after,
                },
                HasImpls::SpaceBefore(_, _) => todo!(),
                HasImpls::SpaceAfter(_, _) => todo!(),
            },
        }
    }
}
