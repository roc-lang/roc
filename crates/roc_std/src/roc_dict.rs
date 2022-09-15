use crate::roc_list::RocList;
use core::{
    fmt::{self, Debug},
    hash::Hash,
    mem::{align_of, ManuallyDrop},
};

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    #[allow(unused)]
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

#[derive(Eq)]
union RocDictItem<K, V> {
    key_first: ManuallyDrop<KeyFirst<K, V>>,
    value_first: ManuallyDrop<ValueFirst<K, V>>,
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
