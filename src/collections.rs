use std::hash::BuildHasherDefault;

pub use fxhash::FxHasher;

#[inline(always)]
pub fn default_hasher() -> BuildHasherDefault<FxHasher> {
    BuildHasherDefault::default()
}

pub type BuildHasher = BuildHasherDefault<FxHasher>;

// Versions of HashMap and HashSet from both std and im_rc
// which use the FNV hasher instead of the default SipHash hasher.
// FNV is faster but less secure; that's fine, since this compiler
// doesn't need cryptographically secure hashes, and also is not a
// server concerned about hash flooding attacks!
pub type MutMap<K, V> = std::collections::HashMap<K, V, BuildHasher>;

pub type MutSet<K> = std::collections::HashSet<K, BuildHasher>;

pub type ImMap<K, V> = im_rc::hashmap::HashMap<K, V, BuildHasher>;

pub type ImSet<K> = im_rc::hashset::HashSet<K, BuildHasher>;
