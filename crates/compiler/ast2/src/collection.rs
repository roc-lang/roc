use bumpalo::collections::Vec;
use bumpalo::Bump;
use std::fmt::Debug;

use crate::spaces::CommentOrNewline;

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
