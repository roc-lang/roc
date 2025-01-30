use bumpalo::collections::String;
use bumpalo::Bump;
use std::hash::{BuildHasherDefault, Hash};

pub use wyhash::WyHash;

#[inline(always)]
pub fn default_hasher() -> BuildHasherDefault<WyHash> {
    BuildHasherDefault::default()
}

pub type BuildHasher = BuildHasherDefault<WyHash>;

// Versions of HashMap and HashSet from both std and im_rc
// which use the FNV hasher instead of the default SipHash hasher.
// FNV is faster but less secure; that's fine, since this compiler
// doesn't need cryptographically secure hashes, and also is not a
// server concerned about hash flooding attacks!
pub type MutMap<K, V> = std::collections::HashMap<K, V, BuildHasher>;

pub type MutSet<K> = std::collections::HashSet<K, BuildHasher>;

pub type ImMap<K, V> = im_rc::hashmap::HashMap<K, V, BuildHasher>;

pub type ImSet<K> = im_rc::hashset::HashSet<K, BuildHasher>;

pub type ImEntry<'a, K, V, S> = im_rc::hashmap::Entry<'a, K, V, S>;

pub type SendMap<K, V> = im::hashmap::HashMap<K, V, BuildHasher>;

pub type SendSet<K> = im::hashset::HashSet<K, BuildHasher>;

pub type BumpMap<K, V> = hashbrown::HashMap<K, V, BuildHasher>;
pub type BumpSet<K> = hashbrown::HashSet<K, BuildHasher>;

pub type FnvMap<K, V> = fnv::FnvHashMap<K, V>;

pub trait BumpMapDefault<'a> {
    fn new_in(arena: &'a bumpalo::Bump) -> Self;

    fn with_capacity_in(capacity: usize, arena: &'a bumpalo::Bump) -> Self;
}

impl<'a, K, V> BumpMapDefault<'a> for BumpMap<K, V> {
    fn new_in(_arena: &'a bumpalo::Bump) -> Self {
        hashbrown::HashMap::with_hasher(default_hasher())
    }

    fn with_capacity_in(capacity: usize, _arena: &'a bumpalo::Bump) -> Self {
        hashbrown::HashMap::with_capacity_and_hasher(capacity, default_hasher())
    }
}

impl<'a, K> BumpMapDefault<'a> for BumpSet<K> {
    fn new_in(_arena: &'a bumpalo::Bump) -> Self {
        hashbrown::HashSet::with_hasher(default_hasher())
    }

    fn with_capacity_in(capacity: usize, _arena: &'a bumpalo::Bump) -> Self {
        hashbrown::HashSet::with_capacity_and_hasher(capacity, default_hasher())
    }
}

pub fn arena_join<'a, I>(arena: &'a Bump, strings: &mut I, join_str: &str) -> String<'a>
where
    I: Iterator<Item = &'a str>,
{
    let mut buf = String::new_in(arena);

    if let Some(first) = strings.next() {
        buf.push_str(first);

        for string in strings {
            buf.reserve(join_str.len() + string.len());

            buf.push_str(join_str);
            buf.push_str(string);
        }
    }

    buf
}

pub fn insert_all<K, V, I>(map: &mut SendMap<K, V>, elems: I)
where
    K: Clone + Eq + Hash,
    V: Clone,
    I: Iterator<Item = (K, V)>,
{
    for (k, v) in elems {
        map.insert(k, v);
    }
}

/// Like im's relative_complement, but for MutMap and with references for arguments.
pub fn relative_complement<K, V>(map: &MutMap<K, V>, other: &MutMap<K, V>) -> MutMap<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone,
{
    let mut answer = MutMap::default();

    for (key, value) in map {
        // Drop any key that exists in the other map,
        // by declining to insert it into the answer.
        if !other.contains_key(key) {
            answer.insert(key.clone(), value.clone());
        }
    }

    answer
}

/// Like intersection_with, except for MutMap and specialized to return
/// a tuple. Also, only clones the values that will be actually returned,
/// rather than cloning everything.
pub fn get_shared<K, V>(map1: &MutMap<K, V>, map2: &MutMap<K, V>) -> MutMap<K, (V, V)>
where
    K: Clone + Eq + Hash,
    V: Clone,
{
    let mut answer = MutMap::default();

    for (key, right_value) in map2 {
        match std::collections::HashMap::get(map1, key) {
            None => (),
            Some(left_value) => {
                answer.insert(key.clone(), (left_value.clone(), right_value.clone()));
            }
        }
    }

    answer
}

/// Like im's union, but for MutMap.
pub fn union<K, V>(mut map: MutMap<K, V>, other: &MutMap<K, V>) -> MutMap<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone,
{
    for (key, value) in other.iter() {
        // If the key exists in both maps, keep the value in the owned one.
        if !map.contains_key(key) {
            map.insert(key.clone(), value.clone());
        }
    }

    map
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct HumanIndex(usize);

impl HumanIndex {
    pub const FIRST: Self = HumanIndex(0);

    pub fn zero_based(i: usize) -> Self {
        HumanIndex(i)
    }

    pub fn to_zero_based(self) -> usize {
        self.0
    }

    pub fn one_based(i: usize) -> Self {
        HumanIndex(i - 1)
    }

    pub fn ordinal(self) -> std::string::String {
        int_to_ordinal(self.0 + 1)
    }
}

fn int_to_ordinal(number: usize) -> std::string::String {
    // NOTE: one-based
    let remainder10 = number % 10;
    let remainder100 = number % 100;

    let ending = match remainder100 {
        11..=13 => "th",
        _ => match remainder10 {
            1 => "st",
            2 => "nd",
            3 => "rd",
            _ => "th",
        },
    };

    format!("{number}{ending}")
}

#[macro_export]
macro_rules! mut_map {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(mut_map!(@single $rest)),*]));

    ($($key:expr => $value:expr,)+) => { mut_map!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        {
            let _cap = mut_map!(@count $($key),*);
            let mut _map = ::std::collections::HashMap::with_capacity_and_hasher(_cap, $crate::all::default_hasher());
            $(
                let _ = _map.insert($key, $value);
            )*
            _map
        }
    };
}
