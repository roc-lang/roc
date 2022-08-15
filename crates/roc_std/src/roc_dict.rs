use crate::roc_list::{self, RocList};
use core::{
    fmt::{self, Debug},
    hash::Hash,
};

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RocDict<K, V>(RocList<(K, V)>);

impl<K, V> RocDict<K, V> {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(RocList::with_capacity(capacity))
    }

    pub fn iter(&self) -> impl Iterator<Item = &(K, V)> {
        self.into_iter()
    }

    pub fn iter_keys(&self) -> impl Iterator<Item = &K> {
        self.0.iter().map(|(key, _)| key)
    }

    pub fn iter_values(&self) -> impl Iterator<Item = &V> {
        self.0.iter().map(|(_, val)| val)
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

impl<K: Debug, V: Debug> Debug for RocDict<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("RocDict ")?;

        f.debug_map()
            .entries(self.iter().map(|(k, v)| (k, v)))
            .finish()
    }
}

impl<K, V> IntoIterator for RocDict<K, V> {
    type Item = (K, V);
    type IntoIter = roc_list::IntoIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a RocDict<K, V> {
    type Item = &'a (K, V);
    type IntoIter = core::slice::Iter<'a, (K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.as_slice().iter()
    }
}
