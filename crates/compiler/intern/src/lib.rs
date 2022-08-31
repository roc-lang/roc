//! Generic interners for concurrent and single-thread use cases.

use std::{
    cell::RefCell,
    hash::{BuildHasher, Hash, Hasher},
    sync::Arc,
};

use parking_lot::{Mutex, RwLock};
use roc_collections::{default_hasher, BumpMap};

/// An interned value.
///
/// When possible, prefer comparing/hashing on the [Interned] representation of a value, rather
/// than the value itself.
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Interned<T>(usize, std::marker::PhantomData<T>);
impl<T> Clone for Interned<T> {
    fn clone(&self) -> Self {
        Self(self.0, Default::default())
    }
}

impl<T> Copy for Interned<T> {}

/// A concurrent interner, suitable for usage between threads.
///
/// The interner does not currently maintain its own arena; you will have to supply
/// values-to-be-interned as allocated in an independent arena.
///
/// If you need a concurrent global interner, you'll likely want each thread to take a
/// [ThreadLocalInterner] via [GlobalInterner::fork], for caching purposes.
///
/// Originally derived from https://gist.github.com/matklad/44ba1a5a6168bc0c26c995131c007907;
/// thank you, Aleksey!
#[derive(Debug)]
pub struct GlobalInterner<'a, K> {
    map: Mutex<BumpMap<&'a K, Interned<K>>>,
    vec: RwLock<Vec<&'a K>>,
}

/// A derivative of a [GlobalInterner] interner that provides caching desirable for
/// thread-local workloads. The only way to get a [ThreadLocalInterner] is via
/// [GlobalInterner::fork].
///
/// All values interned into a [ThreadLocalInterner] are made available in its parent
/// [GlobalInterner], making this suitable for global sharing of interned values.
///
/// Originally derived from https://gist.github.com/matklad/44ba1a5a6168bc0c26c995131c007907;
/// thank you, Aleksey!
#[derive(Debug)]
pub struct ThreadLocalInterner<'a, K> {
    parent: Arc<GlobalInterner<'a, K>>,
    map: BumpMap<&'a K, Interned<K>>,
    /// Cache of interned values from the parent for local access.
    vec: RefCell<Vec<Option<&'a K>>>,
}

/// A single-threaded interner, with no concurrency properties.
///
/// The only way to construct such an interner is to collapse a shared [GlobalInterner] into
/// a [SingleThreadedInterner], via [GlobalInterner::unwrap].
#[derive(Debug)]
pub struct SingleThreadedInterner<'a, K> {
    map: BumpMap<&'a K, Interned<K>>,
    vec: Vec<&'a K>,
}

/// Generic hasher for a value, to be used by all interners.
///
/// This uses the [default_hasher], so interner maps should also rely on [default_hasher].
fn hash<V: Hash>(val: V) -> u64 {
    let mut state = roc_collections::all::BuildHasher::default().build_hasher();
    val.hash(&mut state);
    state.finish()
}

pub trait Interner<'a, K: Hash + Eq> {
    /// Interns a value, returning its interned representation.
    /// If the value has been interned before, the old interned representation will be re-used.
    ///
    /// Note that the provided value must be allocated into an arena of your choosing, but which
    /// must live at least as long as the interner lives.
    // TODO: we should consider maintaining our own arena in the interner, to avoid redundant
    // allocations when values already have interned representations.
    fn insert(&mut self, value: &'a K) -> Interned<K>;

    /// Retrieves a value from the interner.
    fn get(&self, key: Interned<K>) -> &'a K;
}

impl<'a, K: Hash + Eq> GlobalInterner<'a, K> {
    /// Creates a new global interner with the given capacity.
    pub fn with_capacity(cap: usize) -> Arc<GlobalInterner<'a, K>> {
        let map: BumpMap<&'a K, Interned<K>> =
            BumpMap::with_capacity_and_hasher(cap, default_hasher());

        Arc::new(GlobalInterner {
            map: Mutex::new(map),
            vec: RwLock::new(Vec::with_capacity(cap)),
        })
    }

    /// Creates a derivative [ThreadLocalInterner] pointing back to this global interner.
    pub fn fork(self: &Arc<GlobalInterner<'a, K>>) -> ThreadLocalInterner<'a, K> {
        ThreadLocalInterner {
            parent: Arc::clone(self),
            map: Default::default(),
            vec: Default::default(),
        }
    }

    /// Collapses a shared [GlobalInterner] into a [SingleThreadedInterner].
    ///
    /// Returns an [Err] with `self` if there are outstanding references to the [GlobalInterner].
    pub fn unwrap(
        self: Arc<GlobalInterner<'a, K>>,
    ) -> Result<SingleThreadedInterner<'a, K>, Arc<Self>> {
        let GlobalInterner { map, vec } = Arc::try_unwrap(self)?;
        let map = Mutex::into_inner(map);
        let vec = RwLock::into_inner(vec);
        Ok(SingleThreadedInterner { map, vec })
    }

    /// Interns a value with a pre-computed hash.
    /// Prefer calling this when possible, especially from [ThreadLocalInterner], to avoid
    /// re-computing hashes.
    fn insert_hashed(&self, value: &'a K, hash: u64) -> Interned<K> {
        let mut map = self.map.lock();
        let (_, interned) = map
            .raw_entry_mut()
            .from_key_hashed_nocheck(hash, &value)
            .or_insert_with(|| {
                let mut vec = self.vec.write();
                let interned = Interned(vec.len(), Default::default());
                vec.push(value);
                (value, interned)
            });
        *interned
    }

    fn get(&self, interned: Interned<K>) -> &'a K {
        let Interned(index, _) = interned;
        self.vec.read()[index]
    }
}

impl<'a, K: Hash + Eq> ThreadLocalInterner<'a, K> {
    /// Records an interned value in thread-specific storage, for faster access on lookups.
    fn record(&self, key: &'a K, interned: Interned<K>) {
        let mut vec = self.vec.borrow_mut();
        let len = vec.len().max(interned.0 + 1);
        vec.resize(len, None);
        vec[interned.0] = Some(key);
    }
}

impl<'a, K: Hash + Eq> Interner<'a, K> for ThreadLocalInterner<'a, K> {
    fn insert(&mut self, value: &'a K) -> Interned<K> {
        let global = &*self.parent;
        let hash = hash(value);
        let (&mut value, &mut interned) = self
            .map
            .raw_entry_mut()
            .from_key_hashed_nocheck(hash, &value)
            .or_insert_with(|| {
                let interned = global.insert_hashed(value, hash);
                (value, interned)
            });
        self.record(value, interned);
        interned
    }

    fn get(&self, key: Interned<K>) -> &'a K {
        if let Some(Some(value)) = self.vec.borrow().get(key.0) {
            return value;
        }
        let value = self.parent.get(key);
        self.record(value, key);
        value
    }
}

impl<'a, K> SingleThreadedInterner<'a, K> {
    /// Creates a new single threaded interner with the given capacity.
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            map: BumpMap::with_capacity_and_hasher(cap, default_hasher()),
            vec: Vec::with_capacity(cap),
        }
    }

    /// Promotes the [SingleThreadedInterner] back to a [GlobalInterner].
    ///
    /// You should *only* use this if you need to go from a single-threaded to a concurrent context,
    /// or in a case where you explicitly need access to [ThreadLocalInterner]s.
    pub fn into_global(self) -> Arc<GlobalInterner<'a, K>> {
        let SingleThreadedInterner { map, vec } = self;
        Arc::new(GlobalInterner {
            map: Mutex::new(map),
            vec: RwLock::new(vec),
        })
    }
}

impl<'a, K: Hash + Eq> Interner<'a, K> for SingleThreadedInterner<'a, K> {
    fn insert(&mut self, value: &'a K) -> Interned<K> {
        let hash = hash(value);
        let (_, interned) = self
            .map
            .raw_entry_mut()
            .from_key_hashed_nocheck(hash, value)
            .or_insert_with(|| {
                let interned = Interned(self.vec.len(), Default::default());
                self.vec.push(value);
                (value, interned)
            });
        *interned
    }

    fn get(&self, key: Interned<K>) -> &'a K {
        let Interned(index, _) = key;
        self.vec[index]
    }
}
