use std::fmt::Debug;
use std::path::Path;

use crate::header::{AppHeader, HostedHeader, InterfaceHeader, PackageHeader, PlatformHeader};
use crate::ident::Accessor;
use crate::parser::ESingleQuote;
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
pub enum SingleQuoteSegment<'a> {
    Plaintext(&'a str),    // e.g. 'f'
    Unicode(Loc<&'a str>), // e.g. '00A0' in '\u(00A0)'
    EscapedChar(EscapedChar), // e.g. '\n'
                           // No interpolated expressions in single-quoted strings
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EscapedChar {
    Newline,        // \n
    Tab,            // \t
    DoubleQuote,    // \"
    SingleQuote,    // \'
    Backslash,      // \\
    CarriageReturn, // \r
}

impl EscapedChar {
    /// Returns the char that would have been originally parsed to
    pub fn to_parsed_char(self) -> char {
        use EscapedChar::*;

        match self {
            Backslash => '\\',
            SingleQuote => '\'',
            DoubleQuote => '"',
            CarriageReturn => 'r',
            Tab => 't',
            Newline => 'n',
        }
    }

    pub fn unescape(self) -> char {
        use EscapedChar::*;

        match self {
            Backslash => '\\',
            SingleQuote => '\'',
            DoubleQuote => '"',
            CarriageReturn => '\r',
            Tab => '\t',
            Newline => '\n',
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SingleQuoteLiteral<'a> {
    /// The most common case: a plain character with no escapes
    PlainLine(&'a str),
    Line(&'a [SingleQuoteSegment<'a>]),
}

impl<'a> SingleQuoteLiteral<'a> {
    pub fn to_str_in(&self, arena: &'a Bump) -> &'a str {
        match self {
            SingleQuoteLiteral::PlainLine(s) => s,
            SingleQuoteLiteral::Line(segments) => {
                let mut s = String::new_in(arena);
                for segment in *segments {
                    match segment {
                        SingleQuoteSegment::Plaintext(s2) => s.push_str(s2),
                        SingleQuoteSegment::Unicode(loc) => {
                            let s2 = loc.value;
                            let c = u32::from_str_radix(s2, 16).expect("Invalid unicode escape");
                            s.push(char::from_u32(c).expect("Invalid unicode codepoint"));
                        }
                        SingleQuoteSegment::EscapedChar(c) => {
                            s.push(c.unescape());
                        }
                    }
                }
                s.into_bump_str()
            }
        }
    }
}

impl<'a> TryFrom<StrSegment<'a>> for SingleQuoteSegment<'a> {
    type Error = ESingleQuote;

    fn try_from(value: StrSegment<'a>) -> Result<Self, Self::Error> {
        match value {
            StrSegment::Plaintext(s) => Ok(SingleQuoteSegment::Plaintext(s)),
            StrSegment::Unicode(s) => Ok(SingleQuoteSegment::Unicode(s)),
            StrSegment::EscapedChar(s) => Ok(SingleQuoteSegment::EscapedChar(s)),
            StrSegment::Interpolated(_) => Err(ESingleQuote::InterpolationNotAllowed),
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

    /// e.g. `.foo` or `.0`
    AccessorFunction(Accessor<'a>),

    /// Look up exactly one field on a tuple, e.g. `(x, y).1`.
    TupleAccess(&'a Expr<'a>, &'a str),

    // Collection Literals
    List(Collection<'a, &'a Loc<Expr<'a>>>),

    RecordUpdate {
        update: &'a Loc<Expr<'a>>,
        fields: Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>,
    },

    Record(Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    Tuple(Collection<'a, &'a Loc<Expr<'a>>>),

    // Record Builders
    RecordBuilder(Collection<'a, Loc<RecordBuilderField<'a>>>),

    // The name of a file to be ingested directly into a variable.
    IngestedFile(&'a Path, &'a Loc<TypeAnnotation<'a>>),

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
    MultipleRecordBuilders(&'a Loc<Expr<'a>>),
    UnappliedRecordBuilder(&'a Loc<Expr<'a>>),
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
        elems: Collection<'a, Loc<TypeAnnotation<'a>>>,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RecordBuilderField<'a> {
    // A field with a value, e.g. `{ name: "blah" }`
    Value(Loc<&'a str>, &'a [CommentOrNewline<'a>], &'a Loc<Expr<'a>>),

    // A field with a function we can apply to build part of the record, e.g. `{ name: <- apply getName }`
    ApplyValue(
        Loc<&'a str>,
        &'a [CommentOrNewline<'a>],
        &'a [CommentOrNewline<'a>],
        &'a Loc<Expr<'a>>,
    ),

    // A label with no value, e.g. `{ name }` (this is sugar for { name: name })
    LabelOnly(Loc<&'a str>),

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a RecordBuilderField<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a RecordBuilderField<'a>, &'a [CommentOrNewline<'a>]),

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
            LineComment(comment_str) => format!("#{comment_str}"),
            DocComment(comment_str) => format!("##{comment_str}"),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PatternAs<'a> {
    pub spaces_before: &'a [CommentOrNewline<'a>],
    pub identifier: Loc<&'a str>,
}

impl<'a> PatternAs<'a> {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.identifier.value == other.identifier.value
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
    ListRest(Option<(&'a [CommentOrNewline<'a>], PatternAs<'a>)>),

    As(&'a Loc<Pattern<'a>>, PatternAs<'a>),

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

            ListRest(pattern_as) => match other {
                ListRest(other_pattern_as) => match (pattern_as, other_pattern_as) {
                    (Some((_, a)), Some((_, b))) => a.equivalent(b),
                    _ => false,
                },
                _ => false,
            },

            As(pattern, pattern_as) => match other {
                As(other_pattern, other_pattern_as) => {
                    pattern_as.equivalent(other_pattern_as)
                        && pattern.value.equivalent(&other_pattern.value)
                }
                _ => false,
            },

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

impl<'a> Spaceable<'a> for RecordBuilderField<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        RecordBuilderField::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        RecordBuilderField::SpaceAfter(self, spaces)
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

pub trait Malformed {
    /// Returns whether this node is malformed, or contains a malformed node (recursively).
    fn is_malformed(&self) -> bool;
}

impl<'a> Malformed for Module<'a> {
    fn is_malformed(&self) -> bool {
        self.header.is_malformed()
    }
}

impl<'a> Malformed for Header<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            Header::Interface(header) => header.is_malformed(),
            Header::App(header) => header.is_malformed(),
            Header::Package(header) => header.is_malformed(),
            Header::Platform(header) => header.is_malformed(),
            Header::Hosted(header) => header.is_malformed(),
        }
    }
}

impl<'a, T: Malformed> Malformed for Spaces<'a, T> {
    fn is_malformed(&self) -> bool {
        self.item.is_malformed()
    }
}

impl<'a> Malformed for Expr<'a> {
    fn is_malformed(&self) -> bool {
        use Expr::*;

        match self {
            Float(_) |
            Num(_) |
            NonBase10Int { .. } |
            AccessorFunction(_) |
            Var { .. } |
            Underscore(_) |
            Tag(_) |
            OpaqueRef(_) |
            SingleQuote(_) | // This is just a &str - not a bunch of segments
            IngestedFile(_, _) |
            Crash => false,

            Str(inner) => inner.is_malformed(),

            RecordAccess(inner, _) |
            TupleAccess(inner, _) => inner.is_malformed(),

            List(items) => items.is_malformed(),

            RecordUpdate { update, fields } => update.is_malformed() || fields.is_malformed(),
            Record(items) => items.is_malformed(),
            Tuple(items) => items.is_malformed(),

            RecordBuilder(items) => items.is_malformed(),

            Closure(args, body) => args.iter().any(|arg| arg.is_malformed()) || body.is_malformed(),
            Defs(defs, body) => defs.is_malformed() || body.is_malformed(),
            Backpassing(args, call, body) => args.iter().any(|arg| arg.is_malformed()) || call.is_malformed() || body.is_malformed(),
            Expect(condition, continuation) |
            Dbg(condition, continuation) => condition.is_malformed() || continuation.is_malformed(),
            Apply(func, args, _) => func.is_malformed() || args.iter().any(|arg| arg.is_malformed()),
            BinOps(firsts, last) => firsts.iter().any(|(expr, _)| expr.is_malformed()) || last.is_malformed(),
            UnaryOp(expr, _) => expr.is_malformed(),
            If(chain, els) => chain.iter().any(|(cond, body)| cond.is_malformed() || body.is_malformed()) || els.is_malformed(),
            When(cond, branches) => cond.is_malformed() || branches.iter().any(|branch| branch.is_malformed()),

            SpaceBefore(expr, _) |
            SpaceAfter(expr, _) |
            ParensAround(expr) => expr.is_malformed(),

            MalformedIdent(_, _) |
            MalformedClosure |
            PrecedenceConflict(_) |
            MultipleRecordBuilders(_) |
            UnappliedRecordBuilder(_) => true,
        }
    }
}

impl<'a> Malformed for WhenBranch<'a> {
    fn is_malformed(&self) -> bool {
        self.patterns.iter().any(|pat| pat.is_malformed())
            || self.value.is_malformed()
            || self.guard.map(|g| g.is_malformed()).unwrap_or_default()
    }
}

impl<'a, T: Malformed> Malformed for Collection<'a, T> {
    fn is_malformed(&self) -> bool {
        self.iter().any(|item| item.is_malformed())
    }
}

impl<'a> Malformed for StrLiteral<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            StrLiteral::PlainLine(_) => false,
            StrLiteral::Line(segs) => segs.iter().any(|seg| seg.is_malformed()),
            StrLiteral::Block(lines) => lines
                .iter()
                .any(|segs| segs.iter().any(|seg| seg.is_malformed())),
        }
    }
}

impl<'a> Malformed for StrSegment<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            StrSegment::Plaintext(_) | StrSegment::Unicode(_) | StrSegment::EscapedChar(_) => false,
            StrSegment::Interpolated(expr) => expr.is_malformed(),
        }
    }
}

impl<'a, T: Malformed> Malformed for &'a T {
    fn is_malformed(&self) -> bool {
        (*self).is_malformed()
    }
}

impl<T: Malformed> Malformed for Loc<T> {
    fn is_malformed(&self) -> bool {
        self.value.is_malformed()
    }
}

impl<T: Malformed> Malformed for Option<T> {
    fn is_malformed(&self) -> bool {
        self.as_ref()
            .map(|value| value.is_malformed())
            .unwrap_or_default()
    }
}

impl<'a, T: Malformed> Malformed for AssignedField<'a, T> {
    fn is_malformed(&self) -> bool {
        match self {
            AssignedField::RequiredValue(_, _, val) | AssignedField::OptionalValue(_, _, val) => {
                val.is_malformed()
            }
            AssignedField::LabelOnly(_) => false,
            AssignedField::SpaceBefore(field, _) | AssignedField::SpaceAfter(field, _) => {
                field.is_malformed()
            }
            AssignedField::Malformed(_) => true,
        }
    }
}

impl<'a> Malformed for RecordBuilderField<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            RecordBuilderField::Value(_, _, expr)
            | RecordBuilderField::ApplyValue(_, _, _, expr) => expr.is_malformed(),
            RecordBuilderField::LabelOnly(_) => false,
            RecordBuilderField::SpaceBefore(field, _)
            | RecordBuilderField::SpaceAfter(field, _) => field.is_malformed(),
            RecordBuilderField::Malformed(_) => true,
        }
    }
}

impl<'a> Malformed for Pattern<'a> {
    fn is_malformed(&self) -> bool {
        use Pattern::*;

        match self {
            Identifier(_) |
            Tag(_) |
            OpaqueRef(_) => false,
            Apply(func, args) => func.is_malformed() || args.iter().any(|arg| arg.is_malformed()),
            RecordDestructure(items) => items.iter().any(|item| item.is_malformed()),
            RequiredField(_, pat) => pat.is_malformed(),
            OptionalField(_, expr) => expr.is_malformed(),

            NumLiteral(_) |
            NonBase10Literal { .. } |
            Underscore(_) |
            SingleQuote(_) | // This is just a &str - not a bunch of segments
            FloatLiteral(_) => false,

            StrLiteral(lit) => lit.is_malformed(),
            Tuple(items) => items.iter().any(|item| item.is_malformed()),
            List(items) => items.iter().any(|item| item.is_malformed()),
            ListRest(_) =>false,
            As(pat, _) => pat.is_malformed(),
            SpaceBefore(pat, _) |
            SpaceAfter(pat, _) => pat.is_malformed(),

            Malformed(_) |
            MalformedIdent(_, _) |
            QualifiedIdentifier { .. } => true,
        }
    }
}
impl<'a> Malformed for Defs<'a> {
    fn is_malformed(&self) -> bool {
        self.type_defs.iter().any(|def| def.is_malformed())
            || self.value_defs.iter().any(|def| def.is_malformed())
    }
}

impl<'a> Malformed for TypeDef<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            TypeDef::Alias { header, ann } => header.is_malformed() || ann.is_malformed(),
            TypeDef::Opaque {
                header,
                typ,
                derived,
            } => header.is_malformed() || typ.is_malformed() || derived.is_malformed(),
            TypeDef::Ability {
                header,
                loc_has,
                members,
            } => {
                header.is_malformed()
                    || loc_has.is_malformed()
                    || members.iter().any(|member| member.is_malformed())
            }
        }
    }
}

impl<'a> Malformed for AbilityMember<'a> {
    fn is_malformed(&self) -> bool {
        self.typ.is_malformed()
    }
}

impl<'a> Malformed for Has<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            Has::Has => false,
            Has::SpaceBefore(has, _) | Has::SpaceAfter(has, _) => has.is_malformed(),
        }
    }
}

impl<'a> Malformed for HasAbility<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            HasAbility::HasAbility { ability, impls } => {
                ability.is_malformed() || impls.iter().any(|impl_| impl_.is_malformed())
            }
            HasAbility::SpaceBefore(has, _) | HasAbility::SpaceAfter(has, _) => has.is_malformed(),
        }
    }
}

impl<'a> Malformed for HasAbilities<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            HasAbilities::Has(abilities) => abilities.iter().any(|ability| ability.is_malformed()),
            HasAbilities::SpaceBefore(has, _) | HasAbilities::SpaceAfter(has, _) => {
                has.is_malformed()
            }
        }
    }
}

impl<'a> Malformed for HasImpls<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            HasImpls::HasImpls(impls) => impls.iter().any(|ability| ability.is_malformed()),
            HasImpls::SpaceBefore(has, _) | HasImpls::SpaceAfter(has, _) => has.is_malformed(),
        }
    }
}

impl<'a> Malformed for ValueDef<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            ValueDef::Annotation(pat, annotation) => {
                pat.is_malformed() || annotation.is_malformed()
            }
            ValueDef::Body(pat, expr) => pat.is_malformed() || expr.is_malformed(),
            ValueDef::AnnotatedBody {
                ann_pattern,
                ann_type,
                comment: _,
                body_pattern,
                body_expr,
            } => {
                ann_pattern.is_malformed()
                    || ann_type.is_malformed()
                    || body_pattern.is_malformed()
                    || body_expr.is_malformed()
            }
            ValueDef::Dbg {
                condition,
                preceding_comment: _,
            }
            | ValueDef::Expect {
                condition,
                preceding_comment: _,
            }
            | ValueDef::ExpectFx {
                condition,
                preceding_comment: _,
            } => condition.is_malformed(),
        }
    }
}

impl<'a> Malformed for TypeAnnotation<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            TypeAnnotation::Function(args, ret) => {
                args.iter().any(|arg| arg.is_malformed()) || ret.is_malformed()
            }
            TypeAnnotation::Apply(_, _, args) => args.iter().any(|arg| arg.is_malformed()),
            TypeAnnotation::BoundVariable(_)
            | TypeAnnotation::Inferred
            | TypeAnnotation::Wildcard => false,
            TypeAnnotation::As(ty, _, head) => ty.is_malformed() || head.is_malformed(),
            TypeAnnotation::Record { fields, ext } => {
                fields.iter().any(|field| field.is_malformed())
                    || ext.map(|ext| ext.is_malformed()).unwrap_or_default()
            }
            TypeAnnotation::Tuple { elems: fields, ext } => {
                fields.iter().any(|field| field.is_malformed())
                    || ext.map(|ext| ext.is_malformed()).unwrap_or_default()
            }
            TypeAnnotation::TagUnion { ext, tags } => {
                tags.iter().any(|field| field.is_malformed())
                    || ext.map(|ext| ext.is_malformed()).unwrap_or_default()
            }
            TypeAnnotation::Where(ann, clauses) => {
                ann.is_malformed() || clauses.iter().any(|clause| clause.is_malformed())
            }
            TypeAnnotation::SpaceBefore(ty, _) | TypeAnnotation::SpaceAfter(ty, _) => {
                ty.is_malformed()
            }
            TypeAnnotation::Malformed(_) => true,
        }
    }
}

impl<'a> Malformed for TypeHeader<'a> {
    fn is_malformed(&self) -> bool {
        self.vars.iter().any(|var| var.is_malformed())
    }
}

impl<'a> Malformed for Tag<'a> {
    fn is_malformed(&self) -> bool {
        match self {
            Tag::Apply { name: _, args } => args.iter().any(|arg| arg.is_malformed()),
            Tag::SpaceBefore(tag, _) | Tag::SpaceAfter(tag, _) => tag.is_malformed(),
            Tag::Malformed(_) => true,
        }
    }
}

impl<'a> Malformed for HasClause<'a> {
    fn is_malformed(&self) -> bool {
        self.abilities.iter().any(|ability| ability.is_malformed())
    }
}

impl<'a, T: Malformed> Malformed for Spaced<'a, T> {
    fn is_malformed(&self) -> bool {
        match self {
            Spaced::Item(t) => t.is_malformed(),
            Spaced::SpaceBefore(t, _) | Spaced::SpaceAfter(t, _) => t.is_malformed(),
        }
    }
}
