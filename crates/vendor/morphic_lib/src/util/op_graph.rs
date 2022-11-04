use std::borrow::Borrow;

use crate::util::flat_slices::{FlatSlices, Slice};
use crate::util::id_type::{Count, Id};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Node<'a, K, Op> {
    pub op: &'a Op,
    pub inputs: &'a [K],
}

/// Conceptually represents a collection of the form `IdVec<K, (Op, Vec<K>)>`.
///
/// Each tuple `(Op, Vec<K>)` is called a "node", and can be thought of as an "op" together with a
/// list of zero or more "inputs" to that op, which are indices pointing to other nodes.
///
/// The input lists are actually stored in a single contiguous buffer to reduce the number of heap
/// allocations.
///
/// This is essentially just a newtype wrapper around `FlatSlices`.  We use this wrapper to
/// represent op graphs (instead of using `FlatSlices` directly) just so that the names of types,
/// functions, and fields in the API are more meaningful for this use case.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OpGraph<K: Id, Op> {
    inner: FlatSlices<K, Op, K>,
}

impl<K: Id, Op> Default for OpGraph<K, Op> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Id, Op> OpGraph<K, Op> {
    pub fn new() -> Self {
        Self {
            inner: FlatSlices::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn count(&self) -> Count<K> {
        self.inner.count()
    }

    pub fn add_node(&mut self, op: Op, inputs: &[K]) -> K {
        self.inner.push_slice(op, inputs)
    }

    pub fn node<I: Borrow<K>>(&self, idx: I) -> Node<K, Op> {
        let Slice { info, items } = self.inner.get(idx);
        Node {
            op: info,
            inputs: items,
        }
    }
}
