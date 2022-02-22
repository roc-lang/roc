use std::fmt::Debug;

use crate::header::{AppHeader, HostedHeader, InterfaceHeader, PlatformHeader};
use crate::ident::Ident;
use bumpalo::collections::{String, Vec};
use bumpalo::Bump;
use roc_module::called_via::{BinOp, CalledVia, UnaryOp};
use roc_region::all::{Loc, Position, Region};

#[derive(Debug)]
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
pub enum Module<'a> {
    Interface { header: InterfaceHeader<'a> },
    App { header: AppHeader<'a> },
    Platform { header: PlatformHeader<'a> },
    Hosted { header: HostedHeader<'a> },
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

#[derive(Clone, Copy, Debug, PartialEq)]
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
    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access(&'a Expr<'a>, &'a str),
    /// e.g. `.foo`
    AccessorFunction(&'a str),

    // Collection Literals
    List(Collection<'a, &'a Loc<Expr<'a>>>),

    RecordUpdate {
        update: &'a Loc<Expr<'a>>,
        fields: Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>,
    },

    Record(Collection<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    // Lookups
    Var {
        module_name: &'a str, // module_name will only be filled if the original Roc code stated something like `5 + SomeModule.myVar`, module_name will be blank if it was `5 + myVar`
        ident: &'a str,
    },

    Underscore(&'a str),

    // Tags
    GlobalTag(&'a str),
    PrivateTag(&'a str),

    // Reference to an opaque type, e.g. $Opaq
    // TODO(opaques): $->@ in the above comment
    OpaqueRef(&'a str),

    // Pattern Matching
    Closure(&'a [Loc<Pattern<'a>>], &'a Loc<Expr<'a>>),
    /// Multiple defs in a row
    Defs(&'a [&'a Loc<Def<'a>>], &'a Loc<Expr<'a>>),
    Backpassing(&'a [Loc<Pattern<'a>>], &'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>),
    Expect(&'a Loc<Expr<'a>>, &'a Loc<Expr<'a>>),

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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Def<'a> {
    // TODO in canonicalization, validate the pattern; only certain patterns
    // are allowed in annotations.
    Annotation(Loc<Pattern<'a>>, Loc<TypeAnnotation<'a>>),

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
    },

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

    Expect(&'a Loc<Expr<'a>>),

    // Blank Space (e.g. comments, spaces, newlines) before or after a def.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Def<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Def<'a>, &'a [CommentOrNewline<'a>]),

    NotYetImplemented(&'static str),
}

impl<'a> Def<'a> {
    pub fn unroll_spaces_before(&self) -> (&'a [CommentOrNewline<'a>], &Def) {
        let (spaces, def): (&'a [_], &Def) = match self {
            Def::SpaceBefore(def, spaces) => (spaces, def),
            def => (&[], def),
        };
        debug_assert!(!matches!(def, Def::SpaceBefore(_, _)));
        (spaces, def)
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

    /// Inline type alias, e.g. `as List a` in `[ Cons a (List a), Nil ] as List a`
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

    /// A tag union, e.g. `[
    TagUnion {
        /// The row type variable in an open tag union, e.g. the `a` in `[ Foo, Bar ]a`.
        /// This is None if it's a closed tag union like `[ Foo, Bar]`.
        ext: Option<&'a Loc<TypeAnnotation<'a>>>,
        tags: Collection<'a, Loc<Tag<'a>>>,
    },

    /// '_', indicating the compiler should infer the type
    Inferred,

    /// The `*` type variable, e.g. in (List *)
    Wildcard,

    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a TypeAnnotation<'a>, &'a [CommentOrNewline<'a>]),

    /// A malformed type annotation, which will code gen to a runtime error
    Malformed(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Clone, Copy, Debug, PartialEq)]
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
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Pattern<'a> {
    // Identifier
    Identifier(&'a str),

    GlobalTag(&'a str),
    PrivateTag(&'a str),

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
            Ident::GlobalTag(string) => Pattern::GlobalTag(string),
            Ident::PrivateTag(string) => Pattern::PrivateTag(string),
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
            Ident::AccessorFunction(string) => Pattern::Malformed(string),
            Ident::Malformed(string, _problem) => Pattern::Malformed(string),
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
            (Underscore(x), Underscore(y)) => x == y,

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

    pub fn with_items(items: &'a [T]) -> Collection<'a, T> {
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
            *final_comments
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

impl<'a> Spaceable<'a> for Def<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Def::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Def::SpaceAfter(self, spaces)
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
        matches!(self, Expr::GlobalTag(_) | Expr::PrivateTag(_))
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
