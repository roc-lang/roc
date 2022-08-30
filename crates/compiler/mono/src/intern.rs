// Thanks https://gist.github.com/matklad/44ba1a5a6168bc0c26c995131c007907

use parking_lot::{Mutex, RwLock};
use roc_collections::{default_hasher, BumpMap};

use std::{
    cell::RefCell,
    hash::{BuildHasher, Hash, Hasher},
    sync::Arc,
};

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Interned<T>(usize, std::marker::PhantomData<T>);

impl<T> Interned<T> {
    pub unsafe fn fake() -> Self {
        Self(0, Default::default())
    }
}

impl<T> Clone for Interned<T> {
    fn clone(&self) -> Self {
        Self(self.0, Default::default())
    }
}

impl<T> Copy for Interned<T> {}

#[derive(Debug)]
pub struct GlobalInterner<'a, K: 'a> {
    map: Mutex<BumpMap<&'a K, Interned<K>>>,
    vec: RwLock<Vec<&'a K>>,
}

#[derive(Debug)]
pub struct LocalInterner<'a, K: 'a> {
    global: Arc<GlobalInterner<'a, K>>,
    map: BumpMap<&'a K, Interned<K>>,
    vec: RefCell<Vec<Option<&'a K>>>,
}

pub type Interner<'a, K> = GlobalInterner<'a, K>;

impl<'a, K: 'a + Hash + Eq> GlobalInterner<'a, K> {
    pub fn with_capacity(cap: usize) -> Arc<GlobalInterner<'a, K>> {
        let hash_map: BumpMap<&'a K, Interned<K>> =
            BumpMap::with_capacity_and_hasher(cap, default_hasher());
        Arc::new(GlobalInterner {
            map: Mutex::new(hash_map),
            vec: RwLock::new(Vec::with_capacity(cap)),
        })
    }

    pub fn fork(self: &Arc<GlobalInterner<'a, K>>) -> LocalInterner<'a, K> {
        LocalInterner {
            global: Arc::clone(self),
            map: Default::default(),
            vec: Default::default(),
        }
    }

    pub fn insert(&self, key: &'a K) -> Interned<K> {
        self.insert_hashed(key, hash(key))
    }

    fn insert_hashed(&self, key: &'a K, hash: u64) -> Interned<K> {
        let mut map = self.map.lock();
        let (_, interned) = map
            .raw_entry_mut()
            .from_key_hashed_nocheck(hash, &key)
            .or_insert_with(|| {
                let mut vec = self.vec.write();
                let interned = Interned(vec.len(), Default::default());
                vec.push(key);
                (key, interned)
            });
        *interned
    }

    pub fn get(&self, interned: Interned<K>) -> &'a K {
        let Interned(index, _) = interned;
        self.vec.read()[index]
    }
}

impl<'a, K: 'a + Hash + Eq> LocalInterner<'a, K> {
    pub fn insert(&mut self, key: &'a K) -> Interned<K> {
        let global = &*self.global;
        let hash = hash(key);
        let (&mut interned, &mut sym) = self
            .map
            .raw_entry_mut()
            .from_key_hashed_nocheck(hash, &key)
            .or_insert_with(|| {
                let interned = global.insert_hashed(key, hash);
                (key, interned)
            });
        self.record(interned, sym);
        sym
    }

    pub fn get(&self, interned: Interned<K>) -> &'a K {
        if let Some(Some(interned)) = self.vec.borrow().get(interned.0) {
            return interned;
        }
        let key = self.global.get(interned);
        self.record(key, interned);
        key
    }

    fn record(&self, key: &'a K, interned: Interned<K>) {
        let mut vec = self.vec.borrow_mut();
        let len = vec.len().max(interned.0 + 1);
        vec.resize(len, None);
        vec[interned.0] = Some(key);
    }
}

fn hash<V: Hash>(val: V) -> u64 {
    let mut state = roc_collections::all::BuildHasher::default().build_hasher();
    val.hash(&mut state);
    state.finish()
}
