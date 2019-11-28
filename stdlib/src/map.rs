use im_rc::hashmap::HashMap;
use im_rc::vector::Vector;
use wyhash::WyHash;

/// A persistent HashMap which records insertion order and iterates in that order.
pub struct Map<K, V> {
    store: HashMap<K, V, BuildHasherDefault<WyHash>>;
    order: Vector<K>
}

impl<K, V> Map<K, V> {
    pub fn is_empty(self) -> bool {
        self.store.is_empty()
    }
}
