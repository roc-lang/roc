use crate::util::id_type::{Count, Id};
use crate::util::id_vec::IdVec;
use crate::util::replace_none::replace_none;

id_type! {
    BlockFragId(u32);
}

/// A "fragment" of a block, representing a run of consecutive `ValId`s and a pointer to the next
/// fragment.
#[derive(Clone, Debug)]
struct BlockFrag<ValId> {
    /// Inclusive bound
    min_val: ValId,
    /// Exclusive bound
    max_val: ValId,
    prev: Option<BlockFragId>,
    next: Option<BlockFragId>,
}

#[derive(Clone, Debug)]
struct BlockData<BlockInfo> {
    head: BlockFragId,
    tail: BlockFragId,
    info: BlockInfo,
}

/// Conceptually represents a collection of the form `IdVec<BlockId, (Vec<ValId>, BlockInfo)>`.
///
/// Each tuple `(Vec<ValId>, BlockInfo)` is called a "block".
///
/// The blocks are actually stored in a single contiguous buffer to reduce the number of heap
/// allocations, and blocks with long runs of consecutive `ValId`s are stored in a compressed
/// representation.
#[derive(Clone, Debug)]
pub struct Blocks<BlockId: Id, ValId: Id, BlockInfo> {
    frags: IdVec<BlockFragId, BlockFrag<ValId>>,
    blocks: IdVec<BlockId, BlockData<BlockInfo>>,
}

impl<BlockId: Id, ValId: Id, BlockInfo> Blocks<BlockId, ValId, BlockInfo> {
    pub fn new() -> Self {
        Self {
            frags: IdVec::new(),
            blocks: IdVec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.blocks.len()
    }

    pub fn block_count(&self) -> Count<BlockId> {
        self.blocks.count()
    }

    pub fn add_block(&mut self, start_hint: ValId, info: BlockInfo) -> BlockId {
        let frag = BlockFrag {
            min_val: start_hint.clone(),
            max_val: start_hint,
            prev: None,
            next: None,
        };
        let frag_id = self.frags.push(frag);
        self.blocks.push(BlockData {
            head: frag_id,
            tail: frag_id,
            info,
        })
    }

    pub fn add_value(&mut self, block_id: BlockId, val_id: ValId) {
        let block = &mut self.blocks[block_id];
        let tail_frag = &mut self.frags[block.tail];
        let next_val = ValId::from_index_or_panic(tail_frag.max_val.to_index() + 1);
        if tail_frag.max_val.to_index() == val_id.to_index() {
            tail_frag.max_val = next_val;
        } else {
            let new_tail = BlockFrag {
                min_val: val_id.clone(),
                max_val: ValId::from_index_or_panic(val_id.to_index() + 1),
                prev: Some(block.tail),
                next: None,
            };
            let new_tail_id = self.frags.push(new_tail);
            replace_none(&mut self.frags[block.tail].next, new_tail_id).unwrap();
            block.tail = new_tail_id;
        }
    }

    pub fn block_info(&self, block_id: BlockId) -> &BlockInfo {
        &self.blocks[block_id].info
    }

    pub fn block_info_mut(&mut self, block_id: BlockId) -> &mut BlockInfo {
        &mut self.blocks[block_id].info
    }

    pub fn block_values(&self, block_id: BlockId) -> impl Iterator<Item = ValId> + '_ {
        let mut frag = &self.frags[self.blocks[block_id].head];
        let mut val = frag.min_val.clone();
        std::iter::from_fn(move || {
            while val.to_index() >= frag.max_val.to_index() {
                match frag.next {
                    Some(next) => {
                        frag = &self.frags[next];
                        val = frag.min_val.clone();
                    }
                    None => {
                        return None;
                    }
                }
            }
            let this_val = val.clone();
            val = ValId::from_index_unchecked(val.to_index() + 1);
            Some(this_val)
        })
    }

    pub fn block_values_rev(&self, block_id: BlockId) -> impl Iterator<Item = ValId> + '_ {
        let mut frag = &self.frags[self.blocks[block_id].tail];
        let mut val = frag.max_val.clone();
        std::iter::from_fn(move || {
            while val.to_index() <= frag.min_val.to_index() {
                match frag.prev {
                    Some(prev) => {
                        frag = &self.frags[prev];
                        val = frag.max_val.clone();
                    }
                    None => {
                        return None;
                    }
                }
            }
            val = ValId::from_index_unchecked(val.to_index() - 1);
            Some(val.clone())
        })
    }
}
