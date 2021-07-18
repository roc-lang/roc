//! The core IR used for analysis.
//!
//! The IR uses a variant of SSA in which control-dependent values are represented using block
//! parameters instead of phi nodes.

use smallvec::SmallVec;

use crate::api::{CalleeSpecVarId, UpdateModeVarId};
use crate::name_cache::{ConstId, FuncId};
use crate::type_cache::TypeId;
use crate::util::blocks::Blocks;
use crate::util::flat_slices::{FlatSlices, Slice};
use crate::util::op_graph::OpGraph;
use crate::util::strongly_connected::{strongly_connected, SccKind};

/// The "core payload" of a node in the op graph, determining its semantics.
///
/// The `OpKind` of a node is *not* sufficient to uniquely determine either the ids or the types of
/// its inputs or output.  This information is stored separately in the graph.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum OpKind {
    // Variadic inputs
    // Inputs may have any type
    // Output may have any type
    UnknownWith,
    // 1 input
    // Input type must match argument type of callee
    // Output type must match return type of callee
    Call {
        callee_spec_var: CalleeSpecVarId,
        callee: FuncId,
    },
    // 0 inputs
    // Output type must match type of const
    ConstRef {
        const_: ConstId,
    },
    // 0 inputs
    // Output type is heap cell
    NewHeapCell,
    // 1 input
    // Input may have any type
    // Output type is unit
    RecursiveTouch,
    // 1 input
    // Input type is heap cell
    // Output type is unit
    UpdateWriteOnly {
        update_mode_var: UpdateModeVarId,
    },
    // 0 inputs
    // Output type is `Bag<T>` for some `T`
    EmptyBag,
    // 2 inputs: `(bag, to_insert)`
    // `bag` has type `Bag<T>`
    // `to_insert` has type `T`
    // Output type is `Bag<T>`
    BagInsert,
    // 1 input
    // Input type is `Bag<T>`
    // Output type is `T`
    BagGet,
    // 1 input
    // Input type is `Bag<T>`
    // Output type is `(Bag<T>, T)`
    BagRemove,
    // Variadic inputs
    // Input types are `T_1, ..., T_N`
    // Output type is `(T_1, ..., T_N)`
    MakeTuple,
    // 1 input
    // Input type is `(T_1, ..., T_N)`
    // Output type is `T_(field_idx)`
    GetTupleField {
        field_idx: u32,
    },
    // 1 input
    // Input type is `T_(variant_idx)`
    // Output type is `union { T_1, ..., T_N }`
    MakeUnion {
        variant_idx: u32,
    },
    // 1 input
    // Input type is `union { T_1, ..., T_N }`
    // Output type is `T_(variant_idx)`
    UnwrapUnion {
        variant_idx: u32,
    },
    // 1 input
    // Input type is content type of a named type `T`
    // Output type is `T`
    MakeNamed,
    // 1 input
    // Input type is a named type `T`
    // Output type is the content type of `T`
    UnwrapNamed,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum ValueKind {
    Op(OpKind),
    // 0 inputs
    BlockParam,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct ValueInfo {
    pub(crate) kind: ValueKind,
    pub(crate) result_type: TypeId,
}

id_type! {
    pub(crate) ValueId(u32);
}

id_type! {
    pub(crate) BlockId(u32);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum JumpTarget {
    Block(BlockId),
    Ret,
}

pub(crate) const JUMP_TARGETS_INLINE_COUNT: usize = 8;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BlockInfo {
    /// A block may optionally have a single parameter, which serves the same role as a phi node in
    /// SSA.
    ///
    /// Invariant: If `param` is `Some`, it must point to a `BlockParam` value, not an `Op`.
    pub(crate) param: Option<ValueId>,
    /// List of zero or more jump targets to nondeterministically choose from.
    pub(crate) jump_targets: SmallVec<[JumpTarget; JUMP_TARGETS_INLINE_COUNT]>,
    /// Optional argument which will be passed to the chosen jump target.
    pub(crate) target_arg: Option<ValueId>,
}

id_type! {
    /// Identifies a strongly connected component in the control flow graph.
    ///
    /// https://en.wikipedia.org/wiki/Strongly_connected_component
    pub(crate) SccId(u32);
}

#[derive(Clone, Debug)]
pub(crate) struct GraphBuilder {
    values: OpGraph<ValueId, ValueInfo>,
    blocks: Blocks<BlockId, ValueId, BlockInfo>,
}

impl GraphBuilder {
    pub(crate) fn new() -> Self {
        GraphBuilder {
            values: OpGraph::new(),
            blocks: Blocks::new(),
        }
    }

    pub(crate) fn add_op(
        &mut self,
        block: BlockId,
        op_kind: OpKind,
        inputs: &[ValueId],
        result_type: TypeId,
    ) -> ValueId {
        let val_id = self.values.add_node(
            ValueInfo {
                kind: ValueKind::Op(op_kind),
                result_type,
            },
            inputs,
        );
        self.blocks.add_value(block, val_id);
        val_id
    }

    pub(crate) fn add_block(&mut self) -> BlockId {
        self.blocks.add_block(
            self.values.count().0,
            BlockInfo {
                param: None,
                jump_targets: SmallVec::new(),
                target_arg: None,
            },
        )
    }

    pub(crate) fn add_block_with_param(&mut self, param_type: TypeId) -> (BlockId, ValueId) {
        let param_id = self.values.add_node(
            ValueInfo {
                kind: ValueKind::BlockParam,
                result_type: param_type,
            },
            &[],
        );
        let block_id = self.blocks.add_block(
            self.values.count().0,
            BlockInfo {
                param: Some(param_id),
                jump_targets: SmallVec::new(),
                target_arg: None,
            },
        );
        (block_id, param_id)
    }

    pub(crate) fn set_jump_targets(
        &mut self,
        block: BlockId,
        target_arg: Option<ValueId>,
        jump_targets: SmallVec<[JumpTarget; JUMP_TARGETS_INLINE_COUNT]>,
    ) {
        let info = self.blocks.block_info_mut(block);
        info.target_arg = target_arg;
        info.jump_targets = jump_targets;
    }

    pub(crate) fn values(&self) -> &OpGraph<ValueId, ValueInfo> {
        &self.values
    }

    pub(crate) fn blocks(&self) -> &Blocks<BlockId, ValueId, BlockInfo> {
        &self.blocks
    }

    pub(crate) fn build(self, entry_block: BlockId) -> Graph {
        debug_assert!(entry_block < self.blocks.block_count());
        let rev_sccs = strongly_connected(self.blocks.block_count(), |block| {
            self.blocks
                .block_info(block)
                .jump_targets
                .iter()
                .filter_map(|&jump_target| match jump_target {
                    JumpTarget::Ret => None,
                    JumpTarget::Block(target) => Some(target),
                })
        });
        Graph {
            values: self.values,
            blocks: self.blocks,
            entry_block,
            rev_sccs,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Graph {
    values: OpGraph<ValueId, ValueInfo>,
    blocks: Blocks<BlockId, ValueId, BlockInfo>,
    entry_block: BlockId,

    // Invariant: `rev_sccs` must be stored in *reverse* topological order.  If an SCC 'A' can jump
    // to an SCC 'B', then 'A' must appear *after* 'B' in `rev_sccs`.
    //
    // We don't store the SCCs in topological order because control flow graph edges point from
    // *source block* to *target block*, so running Tarjan's algorithm on the control flow graph
    // gives us a reverse topological sort rather than a topological sort.
    rev_sccs: FlatSlices<SccId, SccKind, BlockId>,
}

impl Graph {
    pub(crate) fn values(&self) -> &OpGraph<ValueId, ValueInfo> {
        &self.values
    }

    pub(crate) fn blocks(&self) -> &Blocks<BlockId, ValueId, BlockInfo> {
        &self.blocks
    }

    pub(crate) fn entry_block(&self) -> BlockId {
        self.entry_block
    }

    pub(crate) fn rev_sccs(&self) -> &FlatSlices<SccId, SccKind, BlockId> {
        &self.rev_sccs
    }

    /// Iterate over sccs in topological order.
    ///
    /// IF an SCC 'A' can jump to an SCC 'B', then 'A' is guaranteed to appear *before* 'B' in the
    /// returned iterator.
    pub(crate) fn iter_sccs(&self) -> impl Iterator<Item = Slice<'_, SccKind, BlockId>> + '_ {
        self.rev_sccs
            .count()
            .iter()
            .rev()
            .map(move |scc_id| self.rev_sccs.get(scc_id))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct FuncDef {
    pub(crate) graph: Graph,
}

#[derive(Clone, Debug)]
pub(crate) struct ConstDef {
    pub(crate) graph: Graph,
}
