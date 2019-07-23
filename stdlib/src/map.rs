use im_rc::hashmap::HashMap;
use im_rc::vector::Vector;
use fxhash::FxHasher;

/// A persistent HashMap which records insertion order and iterates in that order.
pub struct Map<K, V> {
    store: HashMap<K, V, BuildHasherDefault<FxHasher>>;
    order: Vector<K>
}

impl<K, V> Map<K, V> {
    pub fn is_empty(self) -> bool {
        self.store.is_empty()
    }
}