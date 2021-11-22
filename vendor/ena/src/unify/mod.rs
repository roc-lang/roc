// Copyright 2012-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Union-find implementation. The main type is `UnificationTable`.
//!
//! You can define your own type for the *keys* in the table, but you
//! must implement `UnifyKey` for that type. The assumption is that
//! keys will be newtyped integers, hence we require that they
//! implement `Copy`.
//!
//! Keys can have values associated with them. The assumption is that
//! these values are cheaply cloneable (ideally, `Copy`), and some of
//! the interfaces are oriented around that assumption. If you just
//! want the classical "union-find" algorithm where you group things
//! into sets, use the `Value` type of `()`.
//!
//! When you have keys with non-trivial values, you must also define
//! how those values can be merged. As part of doing this, you can
//! define the "error" type to return on error; if errors are not
//! possible, use `NoError` (an uninstantiable struct). Using this
//! type also unlocks various more ergonomic methods (e.g., `union()`
//! in place of `unify_var_var()`).
//!
//! The best way to see how it is used is to read the `tests.rs` file;
//! search for e.g. `UnitKey`.

use std::cmp::Ordering;
use std::fmt::{self, Debug};
use std::marker;
use std::ops::Range;

mod backing_vec;
pub use self::backing_vec::{InPlace, UnificationStore};

#[cfg(feature = "persistent")]
pub use self::backing_vec::Persistent;

/// This trait is implemented by any type that can serve as a type
/// variable. We call such variables *unification keys*. For example,
/// this trait is implemented by `IntVid`, which represents integral
/// variables.
///
/// Each key type has an associated value type `V`. For example, for
/// `IntVid`, this is `Option<IntVarValue>`, representing some
/// (possibly not yet known) sort of integer.
///
/// Clients are expected to provide implementations of this trait; you
/// can see some examples in the `test` module.
pub trait UnifyKey: Copy + Clone + Debug + PartialEq {
    type Value: Clone + Debug;

    fn index(&self) -> u32;

    fn from_index(u: u32) -> Self;

    fn tag() -> &'static str;

    /// If true, then `self` should be preferred as root to `other`.
    /// Note that we assume a consistent partial ordering, so
    /// returning true implies that `other.prefer_as_root_to(self)`
    /// would return false.  If there is no ordering between two keys
    /// (i.e., `a.prefer_as_root_to(b)` and `b.prefer_as_root_to(a)`
    /// both return false) then the rank will be used to determine the
    /// root in an optimal way.
    ///
    /// NB. The only reason to implement this method is if you want to
    /// control what value is returned from `find()`. In general, it
    /// is better to let the unification table determine the root,
    /// since overriding the rank can cause execution time to increase
    /// dramatically.
    #[allow(unused_variables)]
    fn order_roots(
        a: Self,
        a_value: &Self::Value,
        b: Self,
        b_value: &Self::Value,
    ) -> Option<(Self, Self)> {
        None
    }
}

/// A struct which can never be instantiated. Used
/// for the error type for infallible cases.
#[derive(Debug)]
pub struct NoError {
    _dummy: (),
}

/// Value of a unification key. We implement Tarjan's union-find
/// algorithm: when two keys are unified, one of them is converted
/// into a "redirect" pointing at the other. These redirects form a
/// DAG: the roots of the DAG (nodes that are not redirected) are each
/// associated with a value of type `V` and a rank. The rank is used
/// to keep the DAG relatively balanced, which helps keep the running
/// time of the algorithm under control. For more information, see
/// <http://en.wikipedia.org/wiki/Disjoint-set_data_structure>.
#[derive(PartialEq, Clone)]
pub struct VarValue<K: UnifyKey> {
    // FIXME pub
    parent: K,           // if equal to self, this is a root
    pub value: K::Value, // value assigned (only relevant to root)
    rank: u32,           // max depth (only relevant to root)
}

impl<K> fmt::Debug for VarValue<K>
where
    K: UnifyKey,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "p: {:?}, c: {:?}", self.parent, self.value)
    }
}

/// Table of unification keys and their values. You must define a key type K
/// that implements the `UnifyKey` trait. Unification tables can be used in two-modes:
///
/// - in-place (`UnificationTable<InPlace<K>>` or `InPlaceUnificationTable<K>`):
///   - This is the standard mutable mode, where the array is modified
///     in place.
///   - To do backtracking, you can employ the `snapshot` and `rollback_to`
///     methods.
/// - persistent (`UnificationTable<Persistent<K>>` or `PersistentUnificationTable<K>`):
///   - In this mode, we use a persistent vector to store the data, so that
///     cloning the table is an O(1) operation.
///   - This implies that ordinary operations are quite a bit slower though.
///   - Requires the `persistent` feature be selected in your Cargo.toml file.
#[derive(Clone, Default)]
pub struct UnificationTable<S: UnificationStore> {
    /// Indicates the current value of each key.
    values: S,
}

impl<S> fmt::Debug for UnificationTable<S>
where
    S: UnificationStore,
    S: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.values.fmt(f)
    }
}

/// A unification table that uses an "in-place" vector.
#[allow(type_alias_bounds)]
pub type InPlaceUnificationTable<K: UnifyKey> = UnificationTable<InPlace<K>>;

/// A unification table that uses a "persistent" vector.
#[cfg(feature = "persistent")]
#[allow(type_alias_bounds)]
pub type PersistentUnificationTable<K: UnifyKey> = UnificationTable<Persistent<K>>;

/// At any time, users may snapshot a unification table.  The changes
/// made during the snapshot may either be *committed* or *rolled back*.
pub struct Snapshot<S: UnificationStore> {
    // Link snapshot to the unification store `S` of the table.
    marker: marker::PhantomData<S>,
    snapshot: S::Snapshot,
}

impl<K: UnifyKey> VarValue<K> {
    fn new_var(key: K, value: K::Value) -> VarValue<K> {
        VarValue::new(key, value, 0)
    }

    fn new(parent: K, value: K::Value, rank: u32) -> VarValue<K> {
        VarValue {
            parent, // this is a root
            value,
            rank,
        }
    }

    fn redirect(&mut self, to: K) {
        self.parent = to;
    }

    fn root(&mut self, rank: u32, value: K::Value) {
        self.rank = rank;
        self.value = value;
    }

    #[inline(always)]
    fn parent(&self, self_key: K) -> Option<K> {
        self.if_not_self(self.parent, self_key)
    }

    fn raw_parent(&self) -> K {
        self.parent
    }

    #[inline(always)]
    fn if_not_self(&self, key: K, self_key: K) -> Option<K> {
        if key == self_key {
            None
        } else {
            Some(key)
        }
    }
}

// We can't use V:LatticeValue, much as I would like to,
// because frequently the pattern is that V=Option<U> for some
// other type parameter U, and we have no way to say
// Option<U>:LatticeValue.

impl<S: UnificationStore> UnificationTable<S> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Starts a new snapshot. Each snapshot must be either
    /// rolled back or committed in a "LIFO" (stack) order.
    pub fn snapshot(&mut self) -> Snapshot<S> {
        Snapshot {
            marker: marker::PhantomData::<S>,
            snapshot: self.values.start_snapshot(),
        }
    }

    /// Reverses all changes since the last snapshot. Also
    /// removes any keys that have been created since then.
    pub fn rollback_to(&mut self, snapshot: Snapshot<S>) {
        debug!("{}: rollback_to()", S::tag());
        self.values.rollback_to(snapshot.snapshot);
    }

    /// Commits all changes since the last snapshot. Of course, they
    /// can still be undone if there is a snapshot further out.
    pub fn commit(&mut self, snapshot: Snapshot<S>) {
        debug!("{}: commit()", S::tag());
        self.values.commit(snapshot.snapshot);
    }

    /// Creates a fresh key with the given value.
    pub fn new_key(&mut self, value: S::Value) -> S::Key {
        let len = self.values.len() as u32;
        let key: S::Key = UnifyKey::from_index(len);
        self.values.push(VarValue::new_var(key, value));
        debug!("{}: created new key: {:?}", S::tag(), key);
        key
    }

    /// Reserve memory for `num_new_keys` to be created. Does not
    /// actually create the new keys; you must then invoke `new_key`.
    pub fn reserve(&mut self, num_new_keys: usize) {
        self.values.reserve(num_new_keys);
    }

    /// Clears all unifications that have been performed, resetting to
    /// the initial state. The values of each variable are given by
    /// the closure.
    pub fn reset_unifications(&mut self, mut value: impl FnMut(S::Key) -> S::Value) {
        self.values.reset_unifications(|i| {
            let key = UnifyKey::from_index(i as u32);
            let value = value(key);
            VarValue::new_var(key, value)
        });
    }

    /// Returns the number of keys created so far.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Returns true iff there have been no keys created yet.
    pub fn is_empty(&self) -> bool {
        self.values.len() == 0
    }

    /// Returns the keys of all variables created since the `snapshot`.
    pub fn vars_since_snapshot(&self, snapshot: &Snapshot<S>) -> Range<S::Key> {
        let range = self.values.values_since_snapshot(&snapshot.snapshot);
        S::Key::from_index(range.start as u32)..S::Key::from_index(range.end as u32)
    }

    /// Obtains the current value for a particular key.
    /// Not for end-users; they can use `probe_value`.
    pub fn value(&self, key: S::Key) -> &VarValue<S::Key> {
        &self.values[key.index() as usize]
    }

    /// Obtains the current value for a particular key.
    /// Not for end-users; they can use `probe_value`.
    pub fn value_mut(&mut self, key: S::Key) -> &mut VarValue<S::Key> {
        &mut self.values[key.index() as usize]
    }

    /// Find the root node for `vid`. This uses the standard
    /// union-find algorithm with path compression:
    /// <http://en.wikipedia.org/wiki/Disjoint-set_data_structure>.
    ///
    /// NB. This is a building-block operation and you would probably
    /// prefer to call `probe` below.
    pub fn get_root_key(&mut self, vid: S::Key) -> S::Key {
        match self.value(vid).parent(vid) {
            None => vid,
            Some(redirect) => {
                let root_key: S::Key = self.get_root_key(redirect);
                if root_key != redirect {
                    // Path compression
                    self.update_value(vid, |value| value.parent = root_key);
                }

                root_key
            }
        }
    }

    pub fn get_root_key_without_compacting(&self, mut vid: S::Key) -> S::Key {
        while let Some(redirect) = self.value(vid).parent(vid) {
            vid = redirect;
        }

        vid
    }

    pub fn is_redirect(&self, vid: S::Key) -> bool {
        self.value(vid).raw_parent() != vid
    }

    pub fn update_value<OP>(&mut self, key: S::Key, op: OP)
    where
        OP: FnOnce(&mut VarValue<S::Key>),
    {
        self.values.update(key.index() as usize, op);
        debug!("Updated variable {:?} to {:?}", key, self.value(key));
    }

    /// Either redirects `node_a` to `node_b` or vice versa, depending
    /// on the relative rank. The value associated with the new root
    /// will be `new_value`.
    ///
    /// NB: This is the "union" operation of "union-find". It is
    /// really more of a building block. If the values associated with
    /// your key are non-trivial, you would probably prefer to call
    /// `unify_var_var` below.
    pub fn unify_roots(&mut self, key_a: S::Key, key_b: S::Key, new_value: S::Value) {
        debug!("unify(key_a={:?}, key_b={:?})", key_a, key_b);

        let rank_a = self.value(key_a).rank;
        let rank_b = self.value(key_b).rank;
        if let Some((new_root, redirected)) = S::Key::order_roots(
            key_a,
            &self.value(key_a).value,
            key_b,
            &self.value(key_b).value,
        ) {
            // compute the new rank for the new root that they chose;
            // this may not be the optimal choice.
            let new_rank = if new_root == key_a {
                debug_assert!(redirected == key_b);
                if rank_a > rank_b {
                    rank_a
                } else {
                    rank_b + 1
                }
            } else {
                debug_assert!(new_root == key_b);
                debug_assert!(redirected == key_a);
                if rank_b > rank_a {
                    rank_b
                } else {
                    rank_a + 1
                }
            };
            self.redirect_root(new_rank, redirected, new_root, new_value);
        } else {
            match rank_a.cmp(&rank_b) {
                Ordering::Greater => {
                    // a has greater rank, so a should become b's parent,
                    // i.e., b should redirect to a.
                    self.redirect_root(rank_a, key_b, key_a, new_value);
                }
                Ordering::Less => {
                    // b has greater rank, so a should redirect to b.
                    self.redirect_root(rank_b, key_a, key_b, new_value);
                }
                Ordering::Equal => {
                    // If equal, redirect one to the other and increment the
                    // other's rank.
                    self.redirect_root(rank_a + 1, key_a, key_b, new_value);
                }
            }
        }
    }

    /// Internal method to redirect `old_root_key` (which is currently
    /// a root) to a child of `new_root_key` (which will remain a
    /// root). The rank and value of `new_root_key` will be updated to
    /// `new_rank` and `new_value` respectively.
    fn redirect_root(
        &mut self,
        new_rank: u32,
        old_root_key: S::Key,
        new_root_key: S::Key,
        new_value: S::Value,
    ) {
        self.update_value(old_root_key, |old_root_value| {
            old_root_value.redirect(new_root_key);
        });
        self.update_value(new_root_key, |new_root_value| {
            new_root_value.root(new_rank, new_value);
        });
    }
}

/// ////////////////////////////////////////////////////////////////////////
/// Public API

impl<'tcx, S, K, V> UnificationTable<S>
where
    S: UnificationStore<Key = K, Value = V>,
    K: UnifyKey<Value = V>,
    V: Clone + Debug,
{
    /// Given two keys, indicates whether they have been unioned together.
    pub fn unioned<K1, K2>(&mut self, a_id: K1, b_id: K2) -> bool
    where
        K1: Into<K>,
        K2: Into<K>,
    {
        self.find(a_id) == self.find(b_id)
    }

    /// Given a key, returns the (current) root key.
    pub fn find<K1>(&mut self, id: K1) -> K
    where
        K1: Into<K>,
    {
        let id = id.into();
        self.get_root_key(id)
    }

    /// Returns the current value for the given key. If the key has
    /// been union'd, this will give the value from the current root.
    #[inline(always)]
    pub fn probe_value<K1>(&mut self, id: K1) -> V
    where
        K1: Into<K>,
    {
        let id = id.into();
        let id = self.get_root_key(id);
        self.value(id).value.clone()
    }

    /// Returns the current value for the given key. If the key has
    /// been union'd, this will give the value from the current root.
    #[inline(always)]
    pub fn probe_value_ref<K1>(&self, id: K1) -> &VarValue<K>
    where
        K1: Into<K>,
    {
        let id = id.into();
        let id = self.get_root_key_without_compacting(id);
        self.value(id)
    }

    /// Returns the current value for the given key. If the key has
    /// been union'd, this will give the value from the current root.
    #[inline(always)]
    pub fn probe_value_ref_mut<K1>(&mut self, id: K1) -> &mut VarValue<K>
    where
        K1: Into<K>,
    {
        let id = id.into();
        let id = self.get_root_key_without_compacting(id);
        self.value_mut(id)
    }

    /// This is for a debug_assert! in solve() only. Do not use it elsewhere!
    #[inline(always)]
    pub fn probe_value_without_compacting<K1>(&self, id: K1) -> V
    where
        K1: Into<K>,
    {
        let id = id.into();
        let id = self.get_root_key_without_compacting(id);

        self.value(id).value.clone()
    }
}
