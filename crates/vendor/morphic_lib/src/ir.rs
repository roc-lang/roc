//! The core IR used for analysis.
//!
//! The IR uses a variant of SSA in which control-dependent values are represented using block
//! parameters instead of phi nodes.

use smallvec::SmallVec;

use crate::api::{CalleeSpecVarId, UpdateModeVarId};
use crate::name_cache::{ConstId, EntryPointId, FuncId, NamedTypeId};
use crate::type_cache::TypeId;
use crate::util::blocks::Blocks;
use crate::util::flat_slices::FlatSlices;
use crate::util::id_type::Count;
use crate::util::id_vec::IdVec;
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Predecessor {
    Block(BlockId),
    Entry,
}

pub(crate) const PREDECESSORS_INLINE_COUNT: usize = 8;
pub(crate) const JUMP_TARGETS_INLINE_COUNT: usize = 8;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BlockInfo {
    /// A block may optionally have a single parameter, which serves the same role as a phi node in
    /// SSA.
    ///
    /// Invariant: If `param` is `Some`, it must point to a `BlockParam` value, not an `Op`.
    pub(crate) param: Option<ValueId>,
    /// Blocks which jump to this block
    pub(crate) predecessors: SmallVec<[Predecessor; PREDECESSORS_INLINE_COUNT]>,
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
    exit_blocks: SmallVec<[BlockId; 1]>,
}

impl GraphBuilder {
    pub(crate) fn new() -> Self {
        GraphBuilder {
            values: OpGraph::new(),
            blocks: Blocks::new(),
            exit_blocks: SmallVec::new(),
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
                predecessors: SmallVec::new(),
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
                predecessors: SmallVec::new(),
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
        for target in &jump_targets {
            match target {
                &JumpTarget::Block(successor) => {
                    self.blocks
                        .block_info_mut(successor)
                        .predecessors
                        .push(Predecessor::Block(block));
                }
                JumpTarget::Ret => {
                    self.exit_blocks.push(block);
                }
            }
        }
        let info = self.blocks.block_info_mut(block);
        info.target_arg = target_arg;
        debug_assert!(info.jump_targets.is_empty());
        info.jump_targets = jump_targets;
    }

    pub(crate) fn values(&self) -> &OpGraph<ValueId, ValueInfo> {
        &self.values
    }

    pub(crate) fn blocks(&self) -> &Blocks<BlockId, ValueId, BlockInfo> {
        &self.blocks
    }

    pub(crate) fn build(
        mut self,
        entry_block: BlockId,
        ret_type: TypeId,
        update_mode_vars: Count<UpdateModeVarId>,
        callee_spec_vars: Count<CalleeSpecVarId>,
    ) -> Graph {
        debug_assert!(entry_block < self.blocks.block_count());
        self.blocks
            .block_info_mut(entry_block)
            .predecessors
            .push(Predecessor::Entry);
        let sccs = strongly_connected(self.blocks.block_count(), |block| {
            self.blocks
                .block_info(block)
                .predecessors
                .iter()
                .filter_map(|&pred| match pred {
                    Predecessor::Entry => None,
                    Predecessor::Block(pred_block) => Some(pred_block),
                })
        });
        Graph {
            values: self.values,
            blocks: self.blocks,
            entry_block,
            exit_blocks: self.exit_blocks,
            ret_type,
            sccs,
            update_mode_vars,
            callee_spec_vars,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Graph {
    values: OpGraph<ValueId, ValueInfo>,
    blocks: Blocks<BlockId, ValueId, BlockInfo>,
    entry_block: BlockId,
    // We use an inline capacity of 1 here because, in the current implementation of `preprocess`,
    // there is always exactly one exit block per function. However, this is no fundamental reason
    // this must be so.
    exit_blocks: SmallVec<[BlockId; 1]>,
    ret_type: TypeId,
    // Invariant: `sccs` is stored in topological order.
    sccs: FlatSlices<SccId, SccKind, BlockId>,
    update_mode_vars: Count<UpdateModeVarId>,
    callee_spec_vars: Count<CalleeSpecVarId>,
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

    pub(crate) fn exit_blocks(&self) -> &[BlockId] {
        &self.exit_blocks
    }

    pub(crate) fn ret_type(&self) -> TypeId {
        self.ret_type
    }

    pub(crate) fn sccs(&self) -> &FlatSlices<SccId, SccKind, BlockId> {
        &self.sccs
    }

    pub(crate) fn update_mode_vars(&self) -> Count<UpdateModeVarId> {
        self.update_mode_vars
    }

    pub(crate) fn callee_spec_vars(&self) -> Count<CalleeSpecVarId> {
        self.callee_spec_vars
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

#[derive(Clone, Debug)]
pub(crate) struct Program {
    pub(crate) named_types: IdVec<NamedTypeId, TypeId>,
    pub(crate) funcs: IdVec<FuncId, FuncDef>,
    pub(crate) consts: IdVec<ConstId, ConstDef>,
    pub(crate) entry_points: IdVec<EntryPointId, FuncId>,
}
