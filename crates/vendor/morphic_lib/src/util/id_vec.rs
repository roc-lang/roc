use std::borrow::Borrow;
use std::fmt::{self, Debug};
use std::iter;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};
use std::slice;
use std::vec;

use crate::util::get2_mut::get2_mut;
use crate::util::id_type::{Count, Id};

#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdVec<K, V> {
    key: PhantomData<K>,
    // Invariant: items.len() <= K::MAX_USIZE
    items: Vec<V>,
}

impl<K: Id + Debug, V: Debug> Debug for IdVec<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(
                self.items
                    .iter()
                    .enumerate()
                    .map(|(idx, val)| (K::from_index_unchecked(idx), val)),
            )
            .finish()
    }
}

#[derive(Clone, Debug)]
pub struct IndexMapped<K, I> {
    key: PhantomData<K>,
    inner: I,
}

impl<K: Id, V, I: Iterator<Item = (usize, V)>> Iterator for IndexMapped<K, I> {
    type Item = (K, V);

    #[inline]
    fn next(&mut self) -> Option<(K, V)> {
        self.inner
            .next()
            .map(|(idx, val)| (K::from_index_unchecked(idx), val))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }

    #[inline]
    fn fold<Acc, G>(self, init: Acc, mut g: G) -> Acc
    where
        G: FnMut(Acc, Self::Item) -> Acc,
    {
        self.inner.fold(init, |acc, (idx, val)| {
            g(acc, (K::from_index_unchecked(idx), val))
        })
    }
}

impl<K: Id, V> IntoIterator for IdVec<K, V> {
    type Item = (K, V);

    type IntoIter = IndexMapped<K, iter::Enumerate<vec::IntoIter<V>>>;

    fn into_iter(self) -> Self::IntoIter {
        IndexMapped {
            key: PhantomData,
            inner: self.items.into_iter().enumerate(),
        }
    }
}

impl<'a, K: Id, V> IntoIterator for &'a IdVec<K, V> {
    type Item = (K, &'a V);

    type IntoIter = IndexMapped<K, iter::Enumerate<slice::Iter<'a, V>>>;

    fn into_iter(self) -> Self::IntoIter {
        IndexMapped {
            key: PhantomData,
            inner: self.items.iter().enumerate(),
        }
    }
}

impl<'a, K: Id, V> IntoIterator for &'a mut IdVec<K, V> {
    type Item = (K, &'a mut V);

    type IntoIter = IndexMapped<K, iter::Enumerate<slice::IterMut<'a, V>>>;

    fn into_iter(self) -> Self::IntoIter {
        IndexMapped {
            key: PhantomData,
            inner: self.items.iter_mut().enumerate(),
        }
    }
}

impl<K: Id, V> IdVec<K, V> {
    pub fn new() -> Self {
        IdVec {
            key: PhantomData,
            items: Vec::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        IdVec {
            key: PhantomData,
            items: Vec::with_capacity(capacity),
        }
    }

    pub fn from_items(items: Vec<V>) -> Self {
        K::assert_in_range(items.len());
        IdVec {
            key: PhantomData,
            items,
        }
    }

    pub fn filled_with(count: Count<K>, mut f: impl FnMut() -> V) -> Self {
        IdVec {
            key: PhantomData,
            items: count.iter().map(|_| f()).collect(),
        }
    }

    pub fn filled_with_indexed(count: Count<K>, f: impl FnMut(K) -> V) -> Self {
        IdVec {
            key: PhantomData,
            items: count.iter().map(f).collect(),
        }
    }

    pub fn items(&self) -> &[V] {
        &self.items
    }

    pub fn items_mut(&mut self) -> &mut [V] {
        &mut self.items
    }

    pub fn into_items(self) -> Vec<V> {
        self.items
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn count(&self) -> Count<K> {
        Count(K::from_index_unchecked(self.len()))
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    #[must_use]
    pub fn push(&mut self, item: V) -> K {
        let id = K::from_index_unchecked(self.len());
        self.items.push(item);
        K::assert_in_range(self.len());
        id
    }

    pub fn truncate(&mut self, len: usize) {
        self.items.truncate(len)
    }

    pub fn iter(&self) -> IndexMapped<K, iter::Enumerate<slice::Iter<V>>> {
        self.into_iter()
    }

    pub fn iter_mut(&mut self) -> IndexMapped<K, iter::Enumerate<slice::IterMut<V>>> {
        self.into_iter()
    }

    pub fn try_zip_exact<'a, U>(
        &'a self,
        other: &'a IdVec<K, U>,
    ) -> Option<impl Iterator<Item = (K, &'a V, &'a U)>> {
        if self.items.len() == other.items.len() {
            Some(
                self.items
                    .iter()
                    .zip(other.items.iter())
                    .enumerate()
                    .map(|(idx, (v1, v2))| (K::from_index_unchecked(idx), v1, v2)),
            )
        } else {
            None
        }
    }

    pub fn into_mapped<W, F: FnMut(K, V) -> W>(self, mut f: F) -> IdVec<K, W> {
        let mapped_items = self.into_iter().map(|(idx, val)| f(idx, val)).collect();
        IdVec::from_items(mapped_items)
    }

    pub fn try_into_mapped<W, E, F: FnMut(K, V) -> Result<W, E>>(
        self,
        mut f: F,
    ) -> Result<IdVec<K, W>, E> {
        let mapped_items = self
            .into_iter()
            .map(|(idx, val)| f(idx, val))
            .collect::<Result<_, _>>()?;

        Ok(IdVec::from_items(mapped_items))
    }

    pub fn map<W, F: FnMut(K, &V) -> W>(&self, mut f: F) -> IdVec<K, W> {
        let mapped_items = self.iter().map(|(idx, val)| f(idx, val)).collect();
        IdVec::from_items(mapped_items)
    }

    pub fn try_map<W, E, F: FnMut(K, &V) -> Result<W, E>>(
        &self,
        mut f: F,
    ) -> Result<IdVec<K, W>, E> {
        let mapped_items = self
            .iter()
            .map(|(idx, val)| f(idx, val))
            .collect::<Result<_, _>>()?;

        Ok(IdVec::from_items(mapped_items))
    }

    pub fn try_from_contiguous(entries: impl Iterator<Item = (K, V)>) -> Option<Self> {
        let mut items = Vec::with_capacity(entries.size_hint().0);
        for (idx, (key, val)) in entries.enumerate() {
            if idx != key.to_index() {
                return None;
            }
            items.push(val);
        }
        Some(Self::from_items(items))
    }

    pub fn get2_mut(&mut self, i: K, j: K) -> Option<(&mut V, &mut V)> {
        get2_mut(&mut self.items, i.to_index(), j.to_index())
    }
}

impl<K: Id, V, I: Borrow<K>> Index<I> for IdVec<K, V> {
    type Output = V;

    fn index(&self, key: I) -> &V {
        &self.items[key.borrow().to_index()]
    }
}

impl<K: Id, V, I: Borrow<K>> IndexMut<I> for IdVec<K, V> {
    fn index_mut(&mut self, key: I) -> &mut V {
        &mut self.items[key.borrow().to_index()]
    }
}
