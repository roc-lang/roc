use std::borrow::Borrow;

use crate::util::id_type::{Count, Id};
use crate::util::id_vec::IdVec;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct NodeData<Op> {
    op: Op,

    /// Determines which slice of the `flat_inputs` buffer contains the inputs to this node.
    ///
    /// If the *previous* op has `inputs_end_idx == a`, and this node has `inputs_end_idx == b`,
    /// then the inputs to this node are given by `flat_inputs[a..b]`.
    inputs_end_idx: usize,
}

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
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OpGraph<K: Id, Op> {
    flat_inputs: Vec<K>,
    nodes: IdVec<K, NodeData<Op>>,
}

impl<K: Id, Op> OpGraph<K, Op> {
    pub fn new() -> Self {
        Self {
            flat_inputs: Vec::new(),
            nodes: IdVec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn count(&self) -> Count<K> {
        self.nodes.count()
    }

    pub fn add_node(&mut self, op: Op, inputs: &[K]) -> K {
        self.flat_inputs.extend_from_slice(inputs);
        let node = NodeData {
            op,
            inputs_end_idx: self.flat_inputs.len(),
        };
        self.nodes.push(node)
    }

    pub fn node<I: Borrow<K>>(&self, idx: I) -> Node<K, Op> {
        let node = &self.nodes[idx.borrow()];
        let inputs_start_idx = if idx.borrow().to_index() == 0 {
            0
        } else {
            self.nodes[K::from_index_unchecked(idx.borrow().to_index() - 1)].inputs_end_idx
        };
        Node {
            op: &node.op,
            inputs: &self.flat_inputs[inputs_start_idx..node.inputs_end_idx],
        }
    }
}
