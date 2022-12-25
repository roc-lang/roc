use roc_region::all::{Loc, Region};

use crate::{AssignedField, Collection, CommentOrNewline, Expr, Pattern, Spaced, Tag};

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
