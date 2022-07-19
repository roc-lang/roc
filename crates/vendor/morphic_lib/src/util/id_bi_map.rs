use std::collections::hash_map::{Entry, HashMap};
use std::hash::Hash;
use std::ops::Deref;

use crate::util::id_type::Id;
use crate::util::id_vec::IdVec;

/// Conceptually represents a collection of the form `IdVec<K, V>` where the `V` values are unique.
///
/// The collection is implemented such that lookups from `V` values to `K` keys are efficient.
#[derive(Clone, Debug)]
pub struct IdBiMap<K: Id, V> {
    key_to_val: IdVec<K, V>,
    val_to_key: HashMap<V, K>,
}

impl<K: Id, V: Hash + Eq + Clone> Default for IdBiMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Id, V> Deref for IdBiMap<K, V> {
    type Target = IdVec<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.key_to_val
    }
}

impl<K: Id, V: Hash + Eq + Clone> IdBiMap<K, V> {
    pub fn new() -> Self {
        IdBiMap {
            key_to_val: IdVec::new(),
            val_to_key: HashMap::new(),
        }
    }

    /// Insert a new unique value into the bi-map.
    ///
    /// If the value is not already present, returns an `Ok` value with the new index of the value.
    ///
    /// If the value is already present, returns an `Err` value with the existing index.
    pub fn insert(&mut self, val: V) -> Result<K, K> {
        match self.val_to_key.entry(val) {
            Entry::Occupied(occupied) => Err(occupied.get().clone()),
            Entry::Vacant(vacant) => {
                let new_index = self.key_to_val.push(vacant.key().clone());
                vacant.insert(new_index.clone());
                Ok(new_index)
            }
        }
    }

    /// Insert a value into the bi-map, or get its key if it is already present.
    pub fn get_or_insert(&mut self, val: V) -> K {
        match self.val_to_key.entry(val) {
            Entry::Occupied(occupied) => occupied.get().clone(),
            Entry::Vacant(vacant) => {
                let new_index = self.key_to_val.push(vacant.key().clone());
                vacant.insert(new_index.clone());
                new_index
            }
        }
    }

    pub fn get_by_val(&self, val: &V) -> Option<K> {
        self.val_to_key.get(val).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (K, &V)> {
        self.key_to_val.iter()
    }
}
