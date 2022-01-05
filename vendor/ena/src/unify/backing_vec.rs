use crate::snapshot_vec as sv;
#[cfg(feature = "persistent")]
use im_rc::Vector;
use std::fmt::{self, Debug};
use std::marker::PhantomData;
use std::ops::{self, Range};

use super::{UnifyKey, VarValue};

#[allow(dead_code)] // rustc BUG
#[allow(type_alias_bounds)]
type Key<S: UnificationStore> = <S as UnificationStore>::Key;

/// Largely internal trait implemented by the unification table
/// backing store types. The most common such type is `InPlace`,
/// which indicates a standard, mutable unification table.
pub trait UnificationStore:
    ops::Index<usize, Output = VarValue<Key<Self>>>
    + ops::IndexMut<usize, Output = VarValue<Key<Self>>>
    + Clone
    + Default
{
    type Key: UnifyKey<Value = Self::Value>;
    type Value: Clone + Debug;
    type Snapshot;

    fn start_snapshot(&mut self) -> Self::Snapshot;

    fn rollback_to(&mut self, snapshot: Self::Snapshot);

    fn commit(&mut self, snapshot: Self::Snapshot);

    fn values_since_snapshot(&self, snapshot: &Self::Snapshot) -> Range<usize>;

    fn reset_unifications(&mut self, value: impl FnMut(usize) -> VarValue<Self::Key>);

    fn len(&self) -> usize;

    fn is_empty(&self) -> bool;

    fn push(&mut self, value: VarValue<Self::Key>);

    fn reserve(&mut self, num_new_values: usize);

    fn update<F>(&mut self, index: usize, op: F)
    where
        F: FnOnce(&mut VarValue<Self::Key>);

    fn tag() -> &'static str {
        Self::Key::tag()
    }
}

/// Backing store for an in-place unification table.
/// Not typically used directly.
#[derive(Clone)]
pub struct InPlace<K: UnifyKey> {
    values: sv::SnapshotVec<Delegate<K>>,
}

impl<K> fmt::Debug for InPlace<K>
where
    K: UnifyKey,
    K: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.values.fmt(f)
    }
}

// HACK(eddyb) manual impl avoids `Default` bound on `K`.
impl<K: UnifyKey> Default for InPlace<K> {
    fn default() -> Self {
        InPlace {
            values: sv::SnapshotVec::new(),
        }
    }
}

impl<K: UnifyKey> UnificationStore for InPlace<K> {
    type Key = K;
    type Value = K::Value;
    type Snapshot = sv::Snapshot;

    #[inline]
    fn start_snapshot(&mut self) -> Self::Snapshot {
        self.values.start_snapshot()
    }

    #[inline]
    fn rollback_to(&mut self, snapshot: Self::Snapshot) {
        self.values.rollback_to(snapshot);
    }

    #[inline]
    fn commit(&mut self, snapshot: Self::Snapshot) {
        self.values.commit(snapshot);
    }

    #[inline]
    fn values_since_snapshot(&self, snapshot: &Self::Snapshot) -> Range<usize> {
        snapshot.value_count..self.len()
    }

    #[inline]
    fn reset_unifications(&mut self, value: impl FnMut(usize) -> VarValue<Self::Key>) {
        self.values.set_all(value);
    }

    fn len(&self) -> usize {
        self.values.len()
    }

    fn is_empty(&self) -> bool {
        self.values.len() == 0
    }

    #[inline]
    fn push(&mut self, value: VarValue<Self::Key>) {
        self.values.push(value);
    }

    #[inline]
    fn reserve(&mut self, num_new_values: usize) {
        self.values.reserve(num_new_values);
    }

    #[inline]
    fn update<F>(&mut self, index: usize, op: F)
    where
        F: FnOnce(&mut VarValue<Self::Key>),
    {
        self.values.update(index, op)
    }
}

impl<K> ops::Index<usize> for InPlace<K>
where
    K: UnifyKey,
{
    type Output = VarValue<K>;
    fn index(&self, index: usize) -> &VarValue<K> {
        &self.values[index]
    }
}

impl<K> ops::IndexMut<usize> for InPlace<K>
where
    K: UnifyKey,
{
    fn index_mut(&mut self, index: usize) -> &mut VarValue<K> {
        &mut self.values[index]
    }
}

#[derive(Copy, Clone, Debug)]
struct Delegate<K>(PhantomData<K>);

impl<K: UnifyKey> sv::SnapshotVecDelegate for Delegate<K> {
    type Value = VarValue<K>;
    type Undo = ();

    fn reverse(_: &mut Vec<VarValue<K>>, _: ()) {}
}

#[cfg(feature = "persistent")]
#[derive(Clone, Debug)]
pub struct Persistent<K: UnifyKey> {
    values: Vector<VarValue<K>>,
}

// HACK(eddyb) manual impl avoids `Default` bound on `K`.
#[cfg(feature = "persistent")]
impl<K: UnifyKey> Default for Persistent<K> {
    fn default() -> Self {
        Persistent {
            values: Vector::new(),
        }
    }
}

#[cfg(feature = "persistent")]
impl<K: UnifyKey> UnificationStore for Persistent<K> {
    type Key = K;
    type Value = K::Value;
    type Snapshot = Self;

    #[inline]
    fn start_snapshot(&mut self) -> Self::Snapshot {
        self.clone()
    }

    #[inline]
    fn rollback_to(&mut self, snapshot: Self::Snapshot) {
        *self = snapshot;
    }

    #[inline]
    fn commit(&mut self, _snapshot: Self::Snapshot) {}

    #[inline]
    fn values_since_snapshot(&self, snapshot: &Self::Snapshot) -> Range<usize> {
        snapshot.len()..self.len()
    }

    #[inline]
    fn reset_unifications(&mut self, mut value: impl FnMut(usize) -> VarValue<Self::Key>) {
        // Without extending dogged, there isn't obviously a more
        // efficient way to do this. But it's pretty dumb. Maybe
        // dogged needs a `map`. [NOTE: revisit in light of replacing dogged with im_rc!]
        for i in 0..self.values.len() {
            self.values[i] = value(i);
        }
    }

    fn len(&self) -> usize {
        self.values.len()
    }

    #[inline]
    fn push(&mut self, value: VarValue<Self::Key>) {
        self.values.push(value);
    }

    #[inline]
    fn reserve(&mut self, _num_new_values: usize) {
        // not obviously relevant to Vector.
    }

    #[inline]
    fn update<F>(&mut self, index: usize, op: F)
    where
        F: FnOnce(&mut VarValue<Self::Key>),
    {
        let p = &mut self.values[index];
        op(p);
    }
}

#[cfg(feature = "persistent")]
impl<K> ops::Index<usize> for Persistent<K>
where
    K: UnifyKey,
{
    type Output = VarValue<K>;
    fn index(&self, index: usize) -> &VarValue<K> {
        &self.values[index]
    }
}
