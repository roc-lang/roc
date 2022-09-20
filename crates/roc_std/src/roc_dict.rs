use crate::roc_list::RocList;
use core::{
    fmt::{self, Debug},
    hash::Hash,
    mem::{align_of, ManuallyDrop},
};

/// At the moment, Roc's Dict is just an association list. Its lookups are O(n) but
/// we haven't grown such big programs that it's a problem yet!
///
/// We do some things in this data structure that only make sense because the
/// memory is managed in Roc:
///
/// 1. We don't implement an [`IntoIterator`] that iterates over owned values,
///    since Roc owns the memory, not rust.
/// 2. We use a union for [`RocDictItem`] instead of just a struct. See the
///    comment on that data structure for why.
#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct RocDict<K, V>(RocList<RocDictItem<K, V>>);

impl<K, V> RocDict<K, V> {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(RocList::with_capacity(capacity))
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.0.iter().map(|item| (item.key(), item.value()))
    }

    pub fn iter_keys(&self) -> impl Iterator<Item = &K> {
        self.0.iter().map(|item| item.key())
    }

    pub fn iter_values(&self) -> impl Iterator<Item = &V> {
        self.0.iter().map(|item| item.value())
    }
}

impl<K: Hash, V> RocDict<K, V> {
    pub fn from_iter<I: Iterator<Item = (K, V)>>(src: I) -> Self {
        let mut ret = Self::with_capacity(src.size_hint().0);

        for (key, val) in src {
            unsafe {
                ret.insert_unchecked(key, val);
            }
        }

        ret
    }

    unsafe fn insert_unchecked(&mut self, _key: K, _val: V) {
        todo!();
    }
}

impl<'a, K: Hash, V> FromIterator<(K, V)> for RocDict<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(into_iter: T) -> Self {
        RocDict::from_iter(into_iter.into_iter())
    }
}

impl<'a, K, V> IntoIterator for &'a RocDict<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = IntoIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            index: 0,
            items: self,
        }
    }
}

pub struct IntoIter<'a, K, V> {
    index: usize,
    items: &'a RocDict<K, V>,
}

impl<'a, K, V> Iterator for IntoIter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        let item = self
            .items
            .0
            .get(self.index)
            .map(|item| (item.key(), item.value()));

        self.index += 1;

        item
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.items.0.len() - self.index;

        (remaining, Some(remaining))
    }
}

impl<K: Debug, V: Debug> Debug for RocDict<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("RocDict ")?;

        f.debug_map()
            .entries(self.iter().map(|(k, v)| (k, v)))
            .finish()
    }
}

/// Roc is constructing these values according to its memory layout rules.
/// Specifically:
///
/// 1. fields with the highest alignment go first
/// 2. then fields are sorted alphabetically
///
/// Taken together, these mean that if we have a value with higher alignment
/// than the key, it'll be first in memory. Otherwise, the key will be first.
/// Fortunately, the total amount of memory doesn't change, so we can use a
/// union and disambiguate by examining the alignment of the key and value.
///
/// However, note that this only makes sense while we're storing KV pairs
/// contiguously in memory. If we separate them at some point, we'll need to
/// change this implementation drastically!
#[derive(Eq)]
union RocDictItem<K, V> {
    key_first: ManuallyDrop<KeyFirst<K, V>>,
    value_first: ManuallyDrop<ValueFirst<K, V>>,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
struct KeyFirst<K, V> {
    key: K,
    value: V,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
struct ValueFirst<K, V> {
    value: V,
    key: K,
}

impl<K, V> RocDictItem<K, V> {
    fn key(&self) -> &K {
        if align_of::<K>() >= align_of::<V>() {
            unsafe { &self.key_first.key }
        } else {
            unsafe { &self.value_first.key }
        }
    }

    fn value(&self) -> &V {
        if align_of::<K>() >= align_of::<V>() {
            unsafe { &self.key_first.value }
        } else {
            unsafe { &self.value_first.value }
        }
    }
}

impl<K, V> Drop for RocDictItem<K, V> {
    fn drop(&mut self) {
        if align_of::<K>() >= align_of::<V>() {
            unsafe { ManuallyDrop::drop(&mut self.key_first) }
        } else {
            unsafe { ManuallyDrop::drop(&mut self.value_first) }
        }
    }
}

impl<K: PartialEq, V: PartialEq> PartialEq for RocDictItem<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key() && self.value() == other.value()
    }
}

impl<K: PartialOrd, V: PartialOrd> PartialOrd for RocDictItem<K, V> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        self.key().partial_cmp(other.key()).map(|key_cmp| {
            match self.value().partial_cmp(other.value()) {
                Some(value_cmp) => key_cmp.then(value_cmp),
                None => key_cmp,
            }
        })
    }
}

impl<K: Ord, V: Ord> Ord for RocDictItem<K, V> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.key()
            .cmp(other.key())
            .then(self.value().cmp(other.value()))
    }
}

impl<K: Hash, V: Hash> Hash for RocDictItem<K, V> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.key().hash(state);
        self.value().hash(state);
    }
}
