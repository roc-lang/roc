use crate::roc_dict::RocDict;
use core::{
    fmt::{self, Debug},
    hash::Hash,
};

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RocSet<T>(RocDict<T, ()>);

impl<T> RocSet<T> {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[allow(unused)]
    pub fn with_capacity(capacity: usize) -> Self {
        Self(RocDict::with_capacity(capacity))
    }

    #[allow(unused)]
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter_keys()
    }
}

impl<T: Hash> RocSet<T> {
    #[allow(unused)]
    pub fn from_iter<I: Iterator<Item = T>>(src: I) -> Self {
        Self(RocDict::from_iter(src.map(|elem| (elem, ()))))
    }
}

impl<T: Debug> Debug for RocSet<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("RocSet ")?;

        f.debug_set().entries(self.iter()).finish()
    }
}
