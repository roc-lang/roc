use std::hash::BuildHasherDefault;

pub use fxhash::FxHasher;

// Versions of HashMap and HashSet from both std and im_rc
// which use the FNV hasher instead of the default SipHash hasher.
// FNV is faster but less secure; that's fine, since this compiler
// doesn't need cryptographically secure hashes, and also is not a
// server concerned about hash flooding attacks!

pub type MutMap<K, V> =
    std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

pub type MutSet<K> =
    std::collections::HashSet<K, BuildHasherDefault<FxHasher>>;

pub type ImMap<K, V> =
    im_rc::hashmap::HashMap<K, V, BuildHasherDefault<FxHasher>>;

pub type ImSet<K> =
    im_rc::hashset::HashSet<K, BuildHasherDefault<FxHasher>>;

// OrdMap equivalents, for naming symmetry.
// Someday we may switch these implementations out.

pub type MutSortedMap<K, V> =
    std::collections::BTreeMap<K, V>;

pub type MutSortedSet<K> =
    std::collections::BTreeSet<K>;

pub type ImSortedMap<K, V> =
    im_rc::ordmap::OrdMap<K, V>;

pub type ImSortedSet<K> =
    im_rc::ordset::OrdSet<K>;
