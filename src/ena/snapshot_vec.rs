// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! A utility class for implementing "snapshottable" things; a snapshottable data structure permits
//! you to take a snapshot (via `start_snapshot`) and then, after making some changes, elect either
//! to rollback to the start of the snapshot or commit those changes.
//!
//! This vector is intended to be used as part of an abstraction, not serve as a complete
//! abstraction on its own. As such, while it will roll back most changes on its own, it also
//! supports a `get_mut` operation that gives you an arbitrary mutable pointer into the vector. To
//! ensure that any changes you make this with this pointer are rolled back, you must invoke
//! `record` to record any changes you make and also supplying a delegate capable of reversing
//! those changes.

use self::UndoLog::*;

use std::fmt;
use std::mem;
use std::ops;

#[derive(Debug)]
pub enum UndoLog<D: SnapshotVecDelegate> {
    /// New variable with given index was created.
    NewElem(usize),

    /// Variable with given index was changed *from* the given value.
    SetElem(usize, D::Value),

    /// Extensible set of actions
    Other(D::Undo),
}

/// A Vec where we have Debug overridden to render the indices like
/// a hashmap, since we really care about those when debugging one of these.
#[derive(Clone)]
struct BackingVec<T>(Vec<T>);

pub struct SnapshotVec<D: SnapshotVecDelegate> {
    values: BackingVec<D::Value>,
    undo_log: Vec<UndoLog<D>>,
    num_open_snapshots: usize,
}

impl<D> fmt::Debug for SnapshotVec<D>
where
    D: SnapshotVecDelegate,
    D: fmt::Debug,
    D::Undo: fmt::Debug,
    D::Value: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("SnapshotVec")
            .field("values", &self.values)
            .field("undo_log", &self.undo_log)
            .field("num_open_snapshots", &self.num_open_snapshots)
            .finish()
    }
}

// Snapshots are tokens that should be created/consumed linearly.
pub struct Snapshot {
    // Number of values at the time the snapshot was taken.
    pub(crate) value_count: usize,
    // Length of the undo log at the time the snapshot was taken.
    undo_len: usize,
}

pub trait SnapshotVecDelegate {
    type Value;
    type Undo;

    fn reverse(values: &mut Vec<Self::Value>, action: Self::Undo);
}

// HACK(eddyb) manual impl avoids `Default` bound on `D`.
impl<D: SnapshotVecDelegate> Default for SnapshotVec<D> {
    fn default() -> Self {
        SnapshotVec {
            values: BackingVec(Vec::new()),
            undo_log: Vec::new(),
            num_open_snapshots: 0,
        }
    }
}

impl<T> fmt::Debug for BackingVec<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{{{")?;

        for (index, elem) in self.0.iter().enumerate() {
            write!(f, "\n    {} => {:?},", index, elem)?;
        }

        if !self.0.is_empty() {
            writeln!(f)?;
        }

        write!(f, "}}}}")
    }
}

impl<D: SnapshotVecDelegate> SnapshotVec<D> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(c: usize) -> SnapshotVec<D> {
        SnapshotVec {
            values: BackingVec(Vec::with_capacity(c)),
            undo_log: Vec::new(),
            num_open_snapshots: 0,
        }
    }

    fn in_snapshot(&self) -> bool {
        self.num_open_snapshots > 0
    }

    pub fn record(&mut self, action: D::Undo) {
        if self.in_snapshot() {
            self.undo_log.push(Other(action));
        }
    }

    pub fn len(&self) -> usize {
        self.values.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.0.len() == 0
    }

    pub fn push(&mut self, elem: D::Value) -> usize {
        let len = self.values.0.len();
        self.values.0.push(elem);

        if self.in_snapshot() {
            self.undo_log.push(NewElem(len));
        }

        len
    }

    pub fn get(&self, index: usize) -> &D::Value {
        &self.values.0[index]
    }

    /// Reserve space for new values, just like an ordinary vec.
    pub fn reserve(&mut self, additional: usize) {
        // This is not affected by snapshots or anything.
        self.values.0.reserve(additional);
    }

    /// Returns a mutable pointer into the vec; whatever changes you make here cannot be undone
    /// automatically, so you should be sure call `record()` with some sort of suitable undo
    /// action.
    pub fn get_mut(&mut self, index: usize) -> &mut D::Value {
        &mut self.values.0[index]
    }

    /// Updates the element at the given index. The old value will saved (and perhaps restored) if
    /// a snapshot is active.
    pub fn set(&mut self, index: usize, new_elem: D::Value) {
        let old_elem = mem::replace(&mut self.values.0[index], new_elem);
        if self.in_snapshot() {
            self.undo_log.push(SetElem(index, old_elem));
        }
    }

    /// Updates all elements. Potentially more efficient -- but
    /// otherwise equivalent to -- invoking `set` for each element.
    pub fn set_all(&mut self, mut new_elems: impl FnMut(usize) -> D::Value) {
        if !self.in_snapshot() {
            for (index, slot) in self.values.0.iter_mut().enumerate() {
                *slot = new_elems(index);
            }
        } else {
            for i in 0..self.values.0.len() {
                self.set(i, new_elems(i));
            }
        }
    }

    pub fn update<OP>(&mut self, index: usize, op: OP)
    where
        OP: FnOnce(&mut D::Value),
        D::Value: Clone,
    {
        if self.in_snapshot() {
            let old_elem = self.values.0[index].clone();
            self.undo_log.push(SetElem(index, old_elem));
        }
        op(&mut self.values.0[index]);
    }

    pub fn start_snapshot(&mut self) -> Snapshot {
        self.num_open_snapshots += 1;
        Snapshot {
            value_count: self.values.0.len(),
            undo_len: self.undo_log.len(),
        }
    }

    pub fn actions_since_snapshot(&self, snapshot: &Snapshot) -> &[UndoLog<D>] {
        &self.undo_log[snapshot.undo_len..]
    }

    fn assert_open_snapshot(&self, snapshot: &Snapshot) {
        // Failures here may indicate a failure to follow a stack discipline.
        assert!(self.undo_log.len() >= snapshot.undo_len);
        assert!(self.num_open_snapshots > 0);
    }

    pub fn rollback_to(&mut self, snapshot: Snapshot) {
        debug!("rollback_to({})", snapshot.undo_len);

        self.assert_open_snapshot(&snapshot);

        while self.undo_log.len() > snapshot.undo_len {
            match self.undo_log.pop().unwrap() {
                NewElem(i) => {
                    self.values.0.pop();
                    assert!(self.values.0.len() == i);
                }

                SetElem(i, v) => {
                    self.values.0[i] = v;
                }

                Other(u) => {
                    D::reverse(&mut self.values.0, u);
                }
            }
        }

        self.num_open_snapshots -= 1;
    }

    /// Commits all changes since the last snapshot. Of course, they
    /// can still be undone if there is a snapshot further out.
    pub fn commit(&mut self, snapshot: Snapshot) {
        debug!("commit({})", snapshot.undo_len);

        self.assert_open_snapshot(&snapshot);

        if self.num_open_snapshots == 1 {
            // The root snapshot. It's safe to clear the undo log because
            // there's no snapshot further out that we might need to roll back
            // to.
            assert!(snapshot.undo_len == 0);
            self.undo_log.clear();
        }

        self.num_open_snapshots -= 1;
    }
}

impl<D: SnapshotVecDelegate> ops::Deref for SnapshotVec<D> {
    type Target = [D::Value];
    fn deref(&self) -> &[D::Value] {
        &*self.values.0
    }
}

impl<D: SnapshotVecDelegate> ops::DerefMut for SnapshotVec<D> {
    fn deref_mut(&mut self) -> &mut [D::Value] {
        &mut *self.values.0
    }
}

impl<D: SnapshotVecDelegate> ops::Index<usize> for SnapshotVec<D> {
    type Output = D::Value;
    fn index(&self, index: usize) -> &D::Value {
        self.get(index)
    }
}

impl<D: SnapshotVecDelegate> ops::IndexMut<usize> for SnapshotVec<D> {
    fn index_mut(&mut self, index: usize) -> &mut D::Value {
        self.get_mut(index)
    }
}

impl<D: SnapshotVecDelegate> Extend<D::Value> for SnapshotVec<D> {
    fn extend<T>(&mut self, iterable: T)
    where
        T: IntoIterator<Item = D::Value>,
    {
        let initial_len = self.values.0.len();
        self.values.0.extend(iterable);
        let final_len = self.values.0.len();

        if self.in_snapshot() {
            self.undo_log.extend((initial_len..final_len).map(NewElem));
        }
    }
}

impl<D: SnapshotVecDelegate> Clone for SnapshotVec<D>
where
    D::Value: Clone,
    D::Undo: Clone,
{
    fn clone(&self) -> Self {
        SnapshotVec {
            values: self.values.clone(),
            undo_log: self.undo_log.clone(),
            num_open_snapshots: self.num_open_snapshots,
        }
    }
}

impl<D: SnapshotVecDelegate> Clone for UndoLog<D>
where
    D::Value: Clone,
    D::Undo: Clone,
{
    fn clone(&self) -> Self {
        match *self {
            NewElem(i) => NewElem(i),
            SetElem(i, ref v) => SetElem(i, v.clone()),
            Other(ref u) => Other(u.clone()),
        }
    }
}

impl SnapshotVecDelegate for i32 {
    type Value = i32;
    type Undo = ();

    fn reverse(_: &mut Vec<i32>, _: ()) {}
}

#[test]
fn basic() {
    let mut vec: SnapshotVec<i32> = SnapshotVec::default();
    assert!(!vec.in_snapshot());
    assert_eq!(vec.len(), 0);
    vec.push(22);
    vec.push(33);
    assert_eq!(vec.len(), 2);
    assert_eq!(*vec.get(0), 22);
    assert_eq!(*vec.get(1), 33);
    vec.set(1, 34);
    assert_eq!(vec.len(), 2);
    assert_eq!(*vec.get(0), 22);
    assert_eq!(*vec.get(1), 34);

    let snapshot = vec.start_snapshot();
    assert!(vec.in_snapshot());

    vec.push(44);
    vec.push(55);
    vec.set(1, 35);
    assert_eq!(vec.len(), 4);
    assert_eq!(*vec.get(0), 22);
    assert_eq!(*vec.get(1), 35);
    assert_eq!(*vec.get(2), 44);
    assert_eq!(*vec.get(3), 55);

    vec.rollback_to(snapshot);
    assert!(!vec.in_snapshot());

    assert_eq!(vec.len(), 2);
    assert_eq!(*vec.get(0), 22);
    assert_eq!(*vec.get(1), 34);
}

#[test]
#[should_panic]
fn out_of_order() {
    let mut vec: SnapshotVec<i32> = SnapshotVec::default();
    vec.push(22);
    let snapshot1 = vec.start_snapshot();
    vec.push(33);
    let snapshot2 = vec.start_snapshot();
    vec.push(44);
    vec.rollback_to(snapshot1); // bogus, but accepted
    vec.rollback_to(snapshot2); // asserts
}

#[test]
fn nested_commit_then_rollback() {
    let mut vec: SnapshotVec<i32> = SnapshotVec::default();
    vec.push(22);
    let snapshot1 = vec.start_snapshot();
    let snapshot2 = vec.start_snapshot();
    vec.set(0, 23);
    vec.commit(snapshot2);
    assert_eq!(*vec.get(0), 23);
    vec.rollback_to(snapshot1);
    assert_eq!(*vec.get(0), 22);
}
