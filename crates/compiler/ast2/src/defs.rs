use roc_collections::soa::{EitherIndex, Index, Slice};
use roc_region::all::{Loc, Region};

use crate::{CommentOrNewline, Expr, HasAbilities, Pattern, Spaced, TypeAnnotation, TypeHeader};

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

pub enum SingleDef<'a> {
    Type(TypeDef<'a>),
    Value(ValueDef<'a>),
}
