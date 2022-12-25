use roc_region::all::{Loc, Region};
use std::fmt::Debug;

use crate::{
    AssignedField, Collection, Expr, Has, HasAbilities, HasAbility, HasImpls, Pattern, Tag,
    TypeAnnotation,
};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spaces<'a, T> {
    pub before: &'a [CommentOrNewline<'a>],
    pub item: T,
    pub after: &'a [CommentOrNewline<'a>],
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpacesBefore<'a, T> {
    pub before: &'a [CommentOrNewline<'a>],
    pub item: T,
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
