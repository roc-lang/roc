use crate::collections::{default_hasher, MutMap};
use hashbrown::raw::RawTable;
use std::hash::Hash;
use std::marker::PhantomData;

#[derive(Debug)]
pub struct Interner<Id, Val>
where
    Id: Copy + From<usize>,
    Val: Eq + Hash,
{
    table: RawTable<Id>,
    val_hashes: MutMap<Id, u64>,

    phantom: PhantomData<Val>,
}

impl<Id, Val> Interner<Id, Val> {
    pub fn get_or_insert(&mut self, val: Val) -> Id {
        let table = self.table;
        let hash = {
            let mut hasher = default_hasher();

            val.hash(hasher);

            hasher.finish()
        };
        let eq = |other| val.eq(other);

        match table.find(hash, eq) {
            Some(bucket) => bucket.read(),
            None => {
                let next_id = table.len();

                self.val_hashes.insert(next_id, hash);
                table.insert(hash, next_id, val, default_hasher());

                next_id
            }
        }
    }

    pub fn get_by_id(&self, id: Id) -> Option<&Val> {
        let table = self.table;

        match self.val_hashes.get(id) {
            Some(hash) => {
                let eq = |other| val.eq(other);

                table.find(hash, eq).map(|bucket| bucket.as_ref())
            }
            None => None,
        }
    }
}
