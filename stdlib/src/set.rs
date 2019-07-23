use im_rc::hashset::HashSet;
use std::hash::{Hash};
use im_rc::vector::Vector;

/// A persistent Set which records insertion order and iterates in that order.
pub struct Set<K> {
    store: HashSet<K>,
    order: Vector<K>
}

impl<K> Set<K>
where K : std::hash::Hash
{
    pub fn is_empty(self) -> bool {
        self.store.is_empty()
    }

    pub fn insert<B>(self, elem: K) -> Set<B>
    {
        let mut new_set: Set<K> = self.clone();

        new_set.store.insert(elem);
        new_set.order.insert(elem);

        new_set
    }

    pub fn map<F, B>(self, transform: F) -> Set<B>
    where F: Fn(K) -> B,
        B: Hash
    {
        let mut new_set: Set<B> = Set::new();

        for elem in self.order.iter() {
            if self.store.contains(elem) {
                new_set.insert(transform(elem))
            }
        }

        new_set
    }
}