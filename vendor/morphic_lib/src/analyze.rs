use smallvec::SmallVec;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::convert::TryInto;
use typed_arena::Arena;

use crate::api;
use crate::ir;
use crate::name_cache::{EntryPointId, FuncId};
use crate::type_cache::{TypeCache, TypeData, TypeId};
use crate::util::flat_slices::FlatSlices;
use crate::util::id_type::Count;
use crate::util::id_vec::IdVec;
use crate::util::norm_pair::NormPair;
use crate::util::op_graph;
use crate::util::replace_none::replace_none;
use crate::util::strongly_connected::{strongly_connected, SccKind};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct SubSlots {
    end_indices: SmallVec<[u32; 10]>,
}

impl SubSlots {
    fn from_slot_counts(slot_counts: impl Iterator<Item = u32>) -> Self {
        let mut total = 0;
        let end_indices = slot_counts
            .map(|count| {
                total += count;
                total
            })
            .collect();
        Self { end_indices }
    }

    fn slot_count(&self) -> u32 {
        self.end_indices.last().cloned().unwrap_or(0)
    }

    /// Returns bounds `a, b` for a range of slot indices `a..b`
    fn sub_slots(&self, index: u32) -> (u32, u32) {
        let start = if index == 0 {
            0
        } else {
            self.end_indices[index as usize - 1]
        };
        let end = self.end_indices[index as usize];
        (start, end)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum TypeSlots {
    Named,
    Tuple { field_slots: SubSlots },
    Union { variant_slots: SubSlots },
    HeapCell,
    Bag { item_slots: u32 },
}

impl TypeSlots {
    fn slot_count(&self) -> u32 {
        match self {
            TypeSlots::Named => 1,
            TypeSlots::Tuple { field_slots } => field_slots.slot_count(),
            TypeSlots::Union { variant_slots } => variant_slots.slot_count(),
            TypeSlots::HeapCell => 1,
            TypeSlots::Bag { item_slots } => *item_slots,
        }
    }
}

#[derive(Clone, Debug)]
struct SlotCache {
    type_cache: TypeCache,
    slots: IdVec<TypeId, TypeSlots>,
}

impl SlotCache {
    fn new(type_cache: TypeCache) -> Self {
        let mut slots: IdVec<_, TypeSlots> = IdVec::new();
        // NOTE: This only works because 'type_cache.types' is guaranteed to assign ids in
        // topological order.
        for (id, type_) in type_cache.types.iter() {
            let this_slots = match type_ {
                TypeData::Named { named: _ } => TypeSlots::Named,
                TypeData::Tuple { fields } => {
                    let field_slots = SubSlots::from_slot_counts(
                        fields.iter().map(|field| slots[field].slot_count()),
                    );
                    TypeSlots::Tuple { field_slots }
                }
                TypeData::Union { variants } => {
                    let variant_slots = SubSlots::from_slot_counts(
                        variants.iter().map(|variant| slots[variant].slot_count()),
                    );
                    TypeSlots::Union { variant_slots }
                }
                TypeData::HeapCell => TypeSlots::HeapCell,
                TypeData::Bag { item } => {
                    let item_slots = slots[item].slot_count();
                    TypeSlots::Bag { item_slots }
                }
            };
            let pushed_id = slots.push(this_slots);
            debug_assert_eq!(pushed_id, id);
        }
        Self { type_cache, slots }
    }

    fn type_cache(&self) -> &TypeCache {
        &self.type_cache
    }

    fn slots(&self) -> &IdVec<TypeId, TypeSlots> {
        &self.slots
    }
}

id_type! {
    HeapCellId(u32);
}

#[derive(Clone)]
struct ForwardState<'a> {
    slots_arena: &'a Arena<HeapCellId>,
    value_slots: IdVec<ir::ValueId, Option<&'a [HeapCellId]>>,
    // Represents value slots for "iteration n - 1" in an SCC
    // TODO: Should this be array-of-structs instead of struct-of-arrays?
    value_slots_inductive: IdVec<ir::ValueId, Option<&'a [HeapCellId]>>,
    call_arg_aliases: IdVec<api::CalleeSpecVarId, Option<HashSet<NormPair<u32>>>>,
    call_arg_origins: IdVec<api::CalleeSpecVarId, Option<SmallVec<[Origin; 4]>>>,
    // TODO: Find a better place to store the data mapping in `calls`
    calls: IdVec<api::CalleeSpecVarId, Option<FuncId>>,
    update_origins: IdVec<api::UpdateModeVarId, Option<Origin>>,
    arg_slots: Option<&'a [HeapCellId]>,
    heap_cells: IdVec<HeapCellId, ForwardData>,
}

type Set<T> = HashSet<T>;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Origin {
    /// This heap cell might have been obtained from a `const_ref` op.
    /// In this case we don't care what arg slots it might have also been obtained from, because we
    /// definitely can't mutate it.
    FromConst,
    /// This heap cell was definitely not obtained from a `const_ref` op.
    /// In this case we care about the (potentially empty) set of arg slots we might have obtained
    /// it from.
    FromArgSlots(Set<u32>),
}

impl Origin {
    pub fn union_with(&mut self, other: &Origin) {
        match (&mut *self, other) {
            (Origin::FromConst, _) => {}
            (Origin::FromArgSlots(_), Origin::FromConst) => *self = Origin::FromConst,
            (Origin::FromArgSlots(slots1), Origin::FromArgSlots(slots2)) => slots1.extend(slots2),
        }
    }
}

impl Default for Origin {
    fn default() -> Self {
        Origin::FromArgSlots(Set::new())
    }
}

#[derive(Clone, Debug)]
struct ForwardData {
    origin: Origin,
    // invariant: does not contain current heap cell id (all heap cells implicitly alias themselves,
    // so storing reflexive alias edges would be redundant).
    // invariant: aliases are symmetric; if we alias another heap cell, that heap cell should also
    // alias us.
    aliases: Set<HeapCellId>,
}

fn result_slot_count(sc: &mut SlotCache, val: &op_graph::Node<ir::ValueId, ir::ValueInfo>) -> u32 {
    sc.slots()[val.op.result_type].slot_count()
}

fn id_result_slot_count(sc: &mut SlotCache, graph: &ir::Graph, val_id: ir::ValueId) -> u32 {
    sc.slots()[graph.values().node(val_id).op.result_type].slot_count()
}

type HeapCellSlotMapping = HashMap<HeapCellId, SmallVec<[(ir::ValueId, u32); 8]>>;

#[derive(Clone, Debug, PartialEq, Eq)]
struct ForwardSccSlotSummary {
    pre_aliases: Set<HeapCellId>,
    inductive_aliases: Set<(ir::ValueId, u32)>,
    internal_aliases: Set<(ir::ValueId, u32)>,
}

type ForwardSccSummary = HashMap<ir::ValueId, SmallVec<[ForwardSccSlotSummary; 2]>>;

fn block_values_inclusive(
    graph: &ir::Graph,
    block: ir::BlockId,
) -> impl Iterator<Item = ir::ValueId> + '_ {
    graph
        .blocks()
        .block_info(block)
        .param
        .iter() // iterator impl on Option
        .cloned()
        .chain(graph.blocks().block_values(block))
}

impl<'a> ForwardState<'a> {
    fn add_heap_cell(&mut self) -> HeapCellId {
        self.heap_cells.push(ForwardData {
            origin: Origin::default(),
            aliases: Set::new(),
        })
    }

    fn add_heap_cells(&mut self, n: u32) -> &'a mut [HeapCellId] {
        self.slots_arena
            .alloc_extend(std::iter::repeat_with(|| self.add_heap_cell()).take(n as usize))
    }

    fn copy_heap_cell(&mut self, cell: HeapCellId, n: u32) -> &'a mut [HeapCellId] {
        self.slots_arena
            .alloc_extend(std::iter::repeat(cell).take(n as usize))
    }

    fn add_alias(&mut self, cell1: HeapCellId, cell2: HeapCellId) {
        if cell1 == cell2 {
            return;
        }
        self.heap_cells[cell1].aliases.insert(cell2);
        self.heap_cells[cell2].aliases.insert(cell1);
    }

    fn copy_aliases(&mut self, src: HeapCellId, dst: HeapCellId) {
        self.add_alias(src, dst);

        // A trick so that we can iterate over `aliases` and call `add_alias` at the same time. At
        // the end of the function we put `src_aliases` back in place.
        //
        // TODO: revist this if we start using "small" sets
        let src_aliases = std::mem::take(&mut self.heap_cells[src].aliases);
        for &other in &src_aliases {
            debug_assert_ne!(other, src);
            self.add_alias(other, dst);
        }
        self.heap_cells[src].aliases = src_aliases;
    }

    fn copy_data(&mut self, src: HeapCellId, dst: HeapCellId) {
        if let Some((src_data, dst_data)) = self.heap_cells.get2_mut(src, dst) {
            src_data.origin.union_with(&dst_data.origin);
        }

        self.copy_aliases(src, dst);
    }

    fn analyze_value(
        &mut self,
        sc: &mut SlotCache,
        ctx: &mut SccAnalysisContext,
        graph: &ir::Graph,
        val_id: ir::ValueId,
    ) {
        let val_node = graph.values().node(val_id);
        let input_slot_arrs: SmallVec<[_; 16]> = val_node
            .inputs
            .iter()
            .map(|input| {
                self.value_slots[input].expect("values should be processed in topological order")
            })
            .collect();
        let op = match &val_node.op.kind {
            ir::ValueKind::Op(op) => op,
            ir::ValueKind::BlockParam => {
                unreachable!("block param should never appear in the values of a block")
            }
        };
        let ret_slots: &[_] = match op {
            ir::OpKind::UnknownWith => {
                let new_cell = self.add_heap_cell();
                self.heap_cells[new_cell].origin = Origin::FromConst;
                for input_slots in input_slot_arrs {
                    for &input_cell in input_slots {
                        self.copy_aliases(input_cell, new_cell);
                    }
                }
                let slot_count = result_slot_count(sc, &val_node);
                self.copy_heap_cell(new_cell, slot_count)
            }

            ir::OpKind::Call {
                callee_spec_var,
                callee,
            } => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                let arg_slots = input_slot_arrs[0];
                // TODO: optimize this entire case!
                let mut heap_cell_slots = HashMap::<HeapCellId, SmallVec<[u32; 4]>>::new();
                for (slot_i, &heap_cell) in arg_slots.iter().enumerate() {
                    heap_cell_slots
                        .entry(heap_cell)
                        .or_insert_with(SmallVec::new)
                        .push(slot_i.try_into().unwrap());
                }
                let mut arg_aliases = HashSet::new();
                for (heap_cell, slot_indices) in &heap_cell_slots {
                    // Wire up to ocurrences of the same heap cell in the argument slots
                    for (i, &slot_i) in slot_indices.iter().enumerate() {
                        for &slot_j in &slot_indices[..i] {
                            arg_aliases.insert(NormPair::new(slot_i, slot_j));
                        }
                    }
                    // Wire up to distinct aliased heap cells in the argument slots
                    for &other in &self.heap_cells[heap_cell].aliases {
                        if let Some(other_slot_indices) = heap_cell_slots.get(&other) {
                            for &this_slot_i in slot_indices {
                                for &other_slot_i in other_slot_indices {
                                    arg_aliases.insert(NormPair::new(this_slot_i, other_slot_i));
                                }
                            }
                        }
                    }
                }

                let ret_slots: &[_] = self.add_heap_cells(result_slot_count(sc, &val_node));

                if let Some(basic_analysis) = ctx.get_analysis(sc, *callee, None) {
                    for (ret_slot_i, slot_analysis) in basic_analysis.ret_slots.iter().enumerate() {
                        let ret_heap_cell = ret_slots[ret_slot_i];
                        if slot_analysis.from_const {
                            self.heap_cells[ret_heap_cell].origin = Origin::FromConst;
                        }
                        // Temporarily violate symmetry invariant
                        for &arg_slot_i in &slot_analysis.arg_aliases {
                            let arg_heap_cell = arg_slots[arg_slot_i as usize];
                            self.heap_cells[ret_heap_cell].aliases.insert(arg_heap_cell);
                            let (arg_heap_cell_data, ret_heap_cell_data) = self
                                .heap_cells
                                .get2_mut(arg_heap_cell, ret_heap_cell)
                                .unwrap();
                            for &alias_of_arg in &arg_heap_cell_data.aliases {
                                ret_heap_cell_data.aliases.insert(alias_of_arg);
                            }
                            ret_heap_cell_data
                                .origin
                                .union_with(&arg_heap_cell_data.origin);
                        }
                        for &other_ret_slot_i in &slot_analysis.ret_aliases {
                            self.heap_cells[ret_heap_cell]
                                .aliases
                                .insert(ret_slots[other_ret_slot_i as usize]);
                        }
                    }
                }

                for &arg_alias in &arg_aliases {
                    if let Some(part_analysis) = ctx.get_analysis(sc, *callee, Some(arg_alias)) {
                        for (ret_slot_i, slot_analysis) in
                            part_analysis.ret_slots.iter().enumerate()
                        {
                            // Temporarily violate symmetry invariant
                            let ret_heap_cell = ret_slots[ret_slot_i];
                            for &arg_slot_i in &slot_analysis.arg_aliases {
                                let arg_heap_cell = arg_slots[arg_slot_i as usize];
                                self.heap_cells[ret_heap_cell].aliases.insert(arg_heap_cell);
                            }
                            for &other_ret_slot_i in &slot_analysis.ret_aliases {
                                self.heap_cells[ret_heap_cell]
                                    .aliases
                                    .insert(ret_slots[other_ret_slot_i as usize]);
                            }
                        }
                    }
                }

                // Restore symmetry invariant
                for &ret_heap_cell in ret_slots {
                    let aliases = std::mem::take(&mut self.heap_cells[ret_heap_cell].aliases);
                    for &other in &aliases {
                        debug_assert_ne!(other, ret_heap_cell);
                        self.heap_cells[other].aliases.insert(ret_heap_cell);
                    }
                    debug_assert!(self.heap_cells[ret_heap_cell].aliases.is_empty());
                    self.heap_cells[ret_heap_cell].aliases = aliases;
                }

                // We don't use 'replace_none' here because we may write these values multiple times
                // during fixed-point iteration.
                self.call_arg_aliases[callee_spec_var] = Some(arg_aliases);
                self.call_arg_origins[callee_spec_var] = Some(
                    arg_slots
                        .iter()
                        .map(|heap_cell| self.heap_cells[heap_cell].origin.clone())
                        .collect(),
                );
                self.calls[callee_spec_var] = Some(*callee);

                ret_slots
            }

            ir::OpKind::ConstRef { const_: _ } => {
                debug_assert_eq!(input_slot_arrs.len(), 0);
                let slot_count = result_slot_count(sc, &val_node);
                let new_heap_cells: &[_] = self.add_heap_cells(slot_count);
                for heap_cell in new_heap_cells {
                    self.heap_cells[heap_cell].origin = Origin::FromConst;
                }
                new_heap_cells
            }

            ir::OpKind::NewHeapCell => {
                debug_assert_eq!(input_slot_arrs.len(), 0);
                let new_cell = self.add_heap_cell();
                std::slice::from_ref(self.slots_arena.alloc(new_cell))
            }

            ir::OpKind::RecursiveTouch => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                &[]
            }

            ir::OpKind::UpdateWriteOnly { update_mode_var } => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                debug_assert_eq!(input_slot_arrs[0].len(), 1);
                // We don't use 'replace_none' here because we may write this value multiple times
                // during fixed-point iteration.
                self.update_origins[*update_mode_var] =
                    Some(self.heap_cells[input_slot_arrs[0][0]].origin.clone());
                &[]
            }

            ir::OpKind::EmptyBag => {
                debug_assert_eq!(input_slot_arrs.len(), 0);
                let slot_count = result_slot_count(sc, &val_node);
                self.add_heap_cells(slot_count)
            }

            ir::OpKind::BagInsert => {
                debug_assert_eq!(input_slot_arrs.len(), 2);
                let slot_count = result_slot_count(sc, &val_node);
                let slots = self.add_heap_cells(slot_count);
                for input_slots in input_slot_arrs {
                    for (&input_cell, &new_cell) in input_slots.iter().zip(slots.iter()) {
                        self.copy_data(input_cell, new_cell);
                    }
                }
                slots
            }

            ir::OpKind::BagGet => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                input_slot_arrs[0]
            }

            ir::OpKind::BagRemove => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                self.slots_arena.alloc_extend(
                    input_slot_arrs[0]
                        .iter()
                        .chain(input_slot_arrs[0].iter())
                        .cloned(),
                )
            }

            ir::OpKind::MakeTuple => self.slots_arena.alloc_extend(
                input_slot_arrs
                    .iter()
                    .flat_map(|slots| slots.iter().cloned()),
            ),

            ir::OpKind::GetTupleField { field_idx } => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                let input_type = graph.values().node(val_node.inputs[0]).op.result_type;
                let field_slots = if let TypeSlots::Tuple { field_slots } = &sc.slots()[input_type]
                {
                    field_slots
                } else {
                    unreachable!()
                };
                let (start, end) = field_slots.sub_slots(*field_idx);
                &input_slot_arrs[0][start as usize..end as usize]
            }

            ir::OpKind::MakeUnion { variant_idx } => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                let variant_slots = if let TypeSlots::Union { variant_slots } =
                    &sc.slots()[val_node.op.result_type]
                {
                    variant_slots
                } else {
                    unreachable!()
                };
                let (start, end) = variant_slots.sub_slots(*variant_idx);
                debug_assert_eq!((end - start) as usize, input_slot_arrs[0].len());
                self.slots_arena
                    .alloc_extend((0..variant_slots.slot_count()).map(|i| {
                        if start <= i && i < end {
                            input_slot_arrs[0][(i - start) as usize]
                        } else {
                            self.add_heap_cell()
                        }
                    }))
            }

            ir::OpKind::UnwrapUnion { variant_idx } => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                let input_type = graph.values().node(val_node.inputs[0]).op.result_type;
                let variant_slots =
                    if let TypeSlots::Union { variant_slots } = &sc.slots()[input_type] {
                        variant_slots
                    } else {
                        unreachable!()
                    };
                let (start, end) = variant_slots.sub_slots(*variant_idx);
                &input_slot_arrs[0][start as usize..end as usize]
            }

            ir::OpKind::MakeNamed => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                let new_cell = self.add_heap_cell();
                for &input_cell in input_slot_arrs[0] {
                    self.copy_data(input_cell, new_cell);
                }
                let slot_count = result_slot_count(sc, &val_node);
                self.copy_heap_cell(new_cell, slot_count)
            }

            ir::OpKind::UnwrapNamed => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                let slot_count = result_slot_count(sc, &val_node);
                self.copy_heap_cell(input_slot_arrs[0][0], slot_count)
            }
        };
        replace_none(&mut self.value_slots[val_id], ret_slots).unwrap();
    }

    fn target_arg_slots(&self, graph: &ir::Graph, pred: ir::Predecessor) -> &'a [HeapCellId] {
        match pred {
            ir::Predecessor::Block(block) => {
                self.value_slots[graph.blocks().block_info(block).target_arg.unwrap()].unwrap()
            }
            ir::Predecessor::Entry => self.arg_slots.unwrap(),
        }
    }

    fn merge_slots<F, I>(
        &mut self,
        slot_count: u32,
        sources: impl Iterator<Item = F>,
    ) -> &'a [HeapCellId]
    where
        F: for<'b> FnMut(&'b Self) -> I,
        I: Iterator<Item = HeapCellId>,
    {
        let min_new_id = self.heap_cells.count();
        let merged_slots: &[_] = self.add_heap_cells(slot_count);
        for mut source in sources {
            // We need to add all of the alias edges up front between slots in the source and slots
            // in the value under construction because we might only know that two slots in the
            // value under construction alias because of their transitive relationship through
            // source slots. For instance, consider the following code:
            //
            // let x = [];
            // let result: (_, _) = choice { (x, x) } or { ([], []) };
            //
            // We know that the two slots in result (the value under construction) might alias
            // because they alias the first and second tuple elements in the first choice branch
            // respectively, and those elements alias.
            let mut i = 0;
            for source_heap_cell in source(self) {
                let merged_heap_cell = merged_slots[i as usize];
                self.copy_data(source_heap_cell, merged_heap_cell);
                i += 1;
            }
            debug_assert_eq!(i, slot_count);

            // Consider the following code:
            //
            // let x = [];
            // let result: (_, _) = choice { (x, []) } or { ([], x) };
            //
            // The first and second slots in result (the value under construction) cannot alias. If
            // we do not remove the edge between the first slot of result and the first slot of the
            // tuple in the first choice branch, then on the next iteration of the loop the
            // algorithm will see a transitive alias edge between them.
            //
            // Note also that we need to remove *all* symmetric edges pointing back from heap cells
            // which predate the value under construction, not just symmetric edges pointing back
            // from the heap cells which appear directly in the predecessor value.  For example,
            // consider the following code:
            //
            // let x = [];
            // let y = /* something that aliases x, but has a distinct heap cell */;
            // let result: (_, _) = choice { (x, []) } or { ([], y) };
            //
            // After processing the first branch of the choice, we need to remove the newly-created
            // edges pointing back from both x *and* y.
            for &merged_heap_cell in merged_slots {
                let merged_aliases = std::mem::take(&mut self.heap_cells[merged_heap_cell].aliases);
                for &other in &merged_aliases {
                    // This removes edges back from heap cells which predate the value under
                    // construction, but does not remove edges between heap cells in the value under
                    // construction.  Preserving edges within the value under construction is
                    // important for handling cases like the following example (also discussed
                    // above) correctly:
                    //
                    // let x = [];
                    // let result: (_, _) = choice { (x, x) } or { ([], []) };
                    if other < min_new_id {
                        // Temporarily violate symmetry invariant
                        self.heap_cells[other].aliases.remove(&merged_heap_cell);
                    }
                }
                debug_assert!(self.heap_cells[merged_heap_cell].aliases.is_empty());
                self.heap_cells[merged_heap_cell].aliases = merged_aliases;
            }
        }
        for &merged_heap_cell in merged_slots {
            let merged_aliases = std::mem::take(&mut self.heap_cells[merged_heap_cell].aliases);
            for &other in &merged_aliases {
                if other < min_new_id {
                    // Restore symmetry invariant
                    self.heap_cells[other].aliases.insert(merged_heap_cell);
                }
            }
            debug_assert!(self.heap_cells[merged_heap_cell].aliases.is_empty());
            self.heap_cells[merged_heap_cell].aliases = merged_aliases;
        }
        merged_slots
    }

    fn analyze_block(
        &mut self,
        sc: &mut SlotCache,
        ctx: &mut SccAnalysisContext,
        graph: &ir::Graph,
        block: ir::BlockId,
    ) {
        let block_info = graph.blocks().block_info(block);
        if let Some(param_id) = block_info.param {
            let param_slots = if block_info.predecessors.len() == 1 {
                self.target_arg_slots(graph, block_info.predecessors[0])
            } else {
                let slot_count = id_result_slot_count(sc, graph, param_id);
                self.merge_slots(
                    slot_count,
                    block_info.predecessors.iter().map(|&pred| {
                        move |this: &Self| this.target_arg_slots(graph, pred).iter().cloned()
                    }),
                )
            };
            replace_none(&mut self.value_slots[param_id], param_slots).unwrap();
        }
        for val_id in graph.blocks().block_values(block) {
            self.analyze_value(sc, ctx, graph, val_id);
        }
    }

    // TODO: Everything to do with SCC analysis can be significantly optimized

    fn heap_cell_slot_mapping(
        &self,
        graph: &ir::Graph,
        blocks: impl Iterator<Item = ir::BlockId>,
    ) -> HeapCellSlotMapping {
        let mut heap_cell_to_slots = HashMap::new();
        for block in blocks {
            for val_id in block_values_inclusive(graph, block) {
                for (i, &heap_cell) in self.value_slots[val_id].unwrap().iter().enumerate() {
                    heap_cell_to_slots
                        .entry(heap_cell)
                        .or_insert_with(SmallVec::new)
                        .push((val_id, i.try_into().unwrap()));
                }
            }
        }
        heap_cell_to_slots
    }

    fn summarize_scc(
        &self,
        graph: &ir::Graph,
        blocks: impl Iterator<Item = ir::BlockId>,
        min_new_id: Count<HeapCellId>,
        heap_cell_slots_inductive: &HeapCellSlotMapping,
        heap_cell_slots_current: &HeapCellSlotMapping,
    ) -> ForwardSccSummary {
        let mut summary = ForwardSccSummary::new();
        for block in blocks {
            for val_id in block_values_inclusive(graph, block) {
                let slot_summaries = self.value_slots[val_id]
                    .unwrap()
                    .iter()
                    .enumerate()
                    .map(|(slot_i, &heap_cell)| {
                        let mut val_summary = ForwardSccSlotSummary {
                            pre_aliases: Set::new(),
                            inductive_aliases: Set::new(),
                            internal_aliases: Set::new(),
                        };
                        let aliased_heap_cells = std::iter::once(heap_cell)
                            .chain(self.heap_cells[heap_cell].aliases.iter().cloned());
                        for aliased in aliased_heap_cells {
                            if aliased < min_new_id {
                                val_summary.pre_aliases.insert(aliased);
                            }
                            for &aliased_slot in heap_cell_slots_current
                                .get(&aliased)
                                .iter()
                                .cloned()
                                .flatten()
                            {
                                if aliased_slot == (val_id, slot_i.try_into().unwrap()) {
                                    continue;
                                }
                                val_summary.internal_aliases.insert(aliased_slot);
                            }
                            for &aliased_slot in heap_cell_slots_inductive
                                .get(&aliased)
                                .iter()
                                .cloned()
                                .flatten()
                            {
                                if aliased_slot == (val_id, slot_i.try_into().unwrap()) {
                                    continue;
                                }
                                val_summary.inductive_aliases.insert(aliased_slot);
                            }
                        }
                        val_summary
                    })
                    .collect();
                summary.insert(val_id, slot_summaries);
            }
        }
        summary
    }

    fn disconnect_heap_cell(&mut self, heap_cell: HeapCellId) {
        let aliases = std::mem::take(&mut self.heap_cells[heap_cell].aliases);
        for &other in &aliases {
            self.heap_cells[other].aliases.remove(&heap_cell);
        }
    }

    fn analyze_block_scc(
        &mut self,
        sc: &mut SlotCache,
        ctx: &mut SccAnalysisContext,
        graph: &ir::Graph,
        scc_id: ir::SccId,
    ) {
        let scc = graph.sccs().get(scc_id);
        match scc.info {
            SccKind::Acyclic => {
                debug_assert!(scc.items.len() == 1);
                self.analyze_block(sc, ctx, graph, scc.items[0]);
            }
            SccKind::Cyclic => {
                let min_new_id = self.heap_cells.count();
                for &block in scc.items {
                    for val_id in block_values_inclusive(graph, block) {
                        let slot_count = id_result_slot_count(sc, graph, val_id);
                        let init_slots = self.add_heap_cells(slot_count);
                        replace_none(&mut self.value_slots[val_id], init_slots).unwrap();
                    }
                }
                let mut prev_iter_summary = None;
                let mut prev_iter_heap_cell_slot_mapping =
                    self.heap_cell_slot_mapping(graph, scc.items.iter().cloned());
                let mut prev_iter_min_new_id = min_new_id;
                loop {
                    let curr_iter_min_new_id = self.heap_cells.count();
                    // Now:
                    // - Main layer stores previous iteration state
                    // - Inductive layer stores irrelevant data
                    for &block in scc.items {
                        for val_id in block_values_inclusive(graph, block) {
                            let slots = std::mem::take(&mut self.value_slots[val_id]);
                            debug_assert!(slots.is_some());
                            self.value_slots_inductive[val_id] = slots;
                        }
                        // Now:
                        // - Main layer stores previous iteration state, except for current block,
                        //   for which it stores 'None'
                        // - Inductive layer stores a mix of irrelevant data and current iteration
                        //   state, except for current block, for which it stores previous iteration
                        //   state
                        self.analyze_block(sc, ctx, graph, block);
                        // Now:
                        // - Main layer stores previous iteration state, except for current block,
                        //   for which it stores current iteration state
                        // - Inductive layer stores a mix of irrelevant data and current iteration
                        //   state, except for current block, for which it stores previous iteration
                        //   state
                        for val_id in block_values_inclusive(graph, block) {
                            std::mem::swap(
                                &mut self.value_slots[val_id],
                                &mut self.value_slots_inductive[val_id],
                            );
                        }
                        // Now:
                        // - Main layer stores previous iteration state
                        // - Inductive layer stores a mix of irrelevant data and current iteration
                        //   state.  In particular, for all blocks processed so far (including this
                        //   one) it stores current iteration state.
                    }
                    // Now:
                    // - Main layer stores previous iteration state
                    // - Inductive layer stores current iteration state
                    for &block in scc.items {
                        for val_id in block_values_inclusive(graph, block) {
                            std::mem::swap(
                                &mut self.value_slots[val_id],
                                &mut self.value_slots_inductive[val_id],
                            );
                        }
                    }
                    // Now:
                    // - Main layer stores current iteration state
                    // - Inductive layer stores previous iteration state
                    let curr_iter_heap_cell_slot_mapping =
                        self.heap_cell_slot_mapping(graph, scc.items.iter().cloned());
                    let curr_iter_summary = self.summarize_scc(
                        graph,
                        scc.items.iter().cloned(),
                        min_new_id,
                        &prev_iter_heap_cell_slot_mapping,
                        &curr_iter_heap_cell_slot_mapping,
                    );
                    if Some(&curr_iter_summary) == prev_iter_summary.as_ref() {
                        break;
                    }

                    // Garbage collect connectsions to irrelevant heap cells from previous iteration
                    for heap_cell in
                        (prev_iter_min_new_id.0 .0..curr_iter_min_new_id.0 .0).map(HeapCellId)
                    {
                        self.disconnect_heap_cell(heap_cell);
                    }

                    prev_iter_summary = Some(curr_iter_summary);
                    prev_iter_heap_cell_slot_mapping = curr_iter_heap_cell_slot_mapping;
                    prev_iter_min_new_id = curr_iter_min_new_id;
                }
            }
        }
    }

    fn analyze_graph(
        slots_arena: &'a Arena<HeapCellId>,
        sc: &mut SlotCache,
        ctx: &mut SccAnalysisContext,
        graph: &ir::Graph,
        arg_alias: Option<NormPair<u32>>,
    ) -> (Self, &'a [HeapCellId]) {
        let mut heap_cells = IdVec::new();
        let arg_slots = graph
            .blocks()
            .block_info(graph.entry_block())
            .param
            .map(|arg_val_id| {
                let slot_count = id_result_slot_count(sc, graph, arg_val_id);
                let arg_slots: &[_] = slots_arena.alloc_extend((0..slot_count).map(|i| {
                    let mut origin_arg_slots = Set::new();
                    origin_arg_slots.insert(i);
                    heap_cells.push(ForwardData {
                        origin: Origin::FromArgSlots(origin_arg_slots),
                        aliases: Set::new(),
                    })
                }));
                if let Some(arg_alias) = arg_alias {
                    let fst = arg_slots[*arg_alias.fst() as usize];
                    let snd = arg_slots[*arg_alias.snd() as usize];
                    heap_cells[fst].aliases.insert(snd);
                    heap_cells[snd].aliases.insert(fst);
                }
                arg_slots
            });

        let mut state = ForwardState {
            slots_arena,
            value_slots: IdVec::filled_with(graph.values().count(), || None),
            value_slots_inductive: IdVec::filled_with(graph.values().count(), || None),
            call_arg_aliases: IdVec::filled_with(graph.callee_spec_vars(), || None),
            call_arg_origins: IdVec::filled_with(graph.callee_spec_vars(), || None),
            calls: IdVec::filled_with(graph.callee_spec_vars(), || None),
            update_origins: IdVec::filled_with(graph.update_mode_vars(), || None),
            arg_slots,
            heap_cells,
        };

        for scc_id in graph.sccs().count().iter() {
            state.analyze_block_scc(sc, ctx, graph, scc_id);
        }

        let ret_slot_count = sc.slots()[graph.ret_type()].slot_count();
        let ret_heap_cells = state.merge_slots(
            ret_slot_count,
            graph.exit_blocks().iter().map(|&block| {
                move |this: &Self| {
                    this.target_arg_slots(graph, ir::Predecessor::Block(block))
                        .iter()
                        .cloned()
                }
            }),
        );

        (state, ret_heap_cells)
    }
}

id_type! {
    StateVersionId(u32);
}

#[derive(Clone, Debug)]
struct BackwardStateVersion {
    parents: SmallVec<[StateVersionId; 8]>,
    // TODO: Is there some way to avoid using a HashMap here?
    overlay: HashMap<HeapCellId, Fate>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Fate {
    DirectTouch,
    Other {
        indirect_touch: bool,
        ret_slots: Set<u32>,
    },
}

impl Fate {
    fn union_with(&mut self, other: &Fate) {
        match (&mut *self, other) {
            (Fate::DirectTouch, _) => {}
            (Fate::Other { .. }, Fate::DirectTouch) => {
                *self = Fate::DirectTouch;
            }
            (
                Fate::Other {
                    indirect_touch: indirect_touch_1,
                    ret_slots: ret_slots_1,
                },
                Fate::Other {
                    indirect_touch: indirect_touch_2,
                    ret_slots: ret_slots_2,
                },
            ) => {
                *indirect_touch_1 = *indirect_touch_1 || *indirect_touch_2;
                ret_slots_1.extend(ret_slots_2);
            }
        }
    }
}

impl Default for Fate {
    fn default() -> Self {
        Fate::Other {
            indirect_touch: false,
            ret_slots: Set::new(),
        }
    }
}

#[derive(Clone, Debug)]
struct CallFates {
    arg_fates: SmallVec<[Fate; 4]>,
    ret_fates: SmallVec<[Fate; 4]>,
}

#[derive(Clone)]
struct BackwardState<'a, 'b> {
    forward_state: &'b ForwardState<'a>,
    versions: IdVec<StateVersionId, BackwardStateVersion>,
    ret_version: StateVersionId,
    ret_slots: &'a [HeapCellId],
    block_versions: IdVec<ir::BlockId, Option<StateVersionId>>,
    update_fates: IdVec<api::UpdateModeVarId, Option<Fate>>,
    call_fates: IdVec<api::CalleeSpecVarId, Option<CallFates>>,
}

impl<'a, 'b> BackwardState<'a, 'b> {
    fn fate(&mut self, version: StateVersionId, heap_cell: HeapCellId) -> &mut Fate {
        // TODO: Is there some way to appease the borrow checker here and avoid the redundant hash?
        if self.versions[version].overlay.contains_key(&heap_cell) {
            return self.versions[version].overlay.get_mut(&heap_cell).unwrap();
        }
        match &self.versions[version].parents as &[_] {
            &[parent] => {
                let fate = self.fate(parent, heap_cell).clone();
                self.versions[version]
                    .overlay
                    .entry(heap_cell)
                    .or_insert(fate) // always inserts
            }
            parents => {
                let num_parents = parents.len();
                let mut fate = Fate::default();
                for parent_i in 0..num_parents {
                    let parent = self.versions[version].parents[parent_i];
                    let parent_fate = self.fate(parent, heap_cell);
                    fate.union_with(parent_fate);
                }
                self.versions[version]
                    .overlay
                    .entry(heap_cell)
                    .or_insert(fate) // always inserts
            }
        }
    }

    fn copy_fate(&mut self, version: StateVersionId, src: HeapCellId, dst: HeapCellId) {
        if src == dst {
            return;
        }
        let mut dst_fate = std::mem::take(self.fate(version, dst));
        dst_fate.union_with(self.fate(version, src));
        debug_assert_eq!(self.fate(version, dst), &Fate::default());
        *self.fate(version, dst) = dst_fate;
    }

    fn analyze_value(
        &mut self,
        sc: &mut SlotCache,
        ctx: &mut SccAnalysisContext,
        version: StateVersionId,
        graph: &ir::Graph,
        val_id: ir::ValueId,
    ) {
        let val_node = graph.values().node(val_id);
        let input_slot_arrs: SmallVec<[_; 16]> = val_node
            .inputs
            .iter()
            .map(|input| self.forward_state.value_slots[input].unwrap())
            .collect();
        let ret_slots = self.forward_state.value_slots[val_id].unwrap();
        let op = match &val_node.op.kind {
            ir::ValueKind::Op(op) => op,
            ir::ValueKind::BlockParam => {
                unreachable!("block param should never appear in the values of a block")
            }
        };
        // TODO: Many of these cases are essentially no-ops, because the implementation details of
        // the forward pass make it such that in these cases copy_fate is always called with an
        // identical src and dst.  Maybe we shouldn't bother with those cases?
        match op {
            ir::OpKind::Call {
                callee_spec_var,
                callee,
            } => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                let arg_slots = input_slot_arrs[0];
                let arg_aliases = self.forward_state.call_arg_aliases[callee_spec_var]
                    .as_ref()
                    .unwrap();
                let arg_fates = arg_slots
                    .iter()
                    .map(|&heap_cell| self.fate(version, heap_cell).clone())
                    .collect();
                let ret_fates = ret_slots
                    .iter()
                    .map(|&heap_cell| self.fate(version, heap_cell).clone())
                    .collect();
                // TODO: do we even need to consider analyses arising from non-'None' arg aliases
                // here?
                for arg_alias in std::iter::once(None).chain(arg_aliases.iter().cloned().map(Some))
                {
                    if let Some(analysis) = ctx.get_analysis(sc, *callee, arg_alias) {
                        for (slot_i, slot_analysis) in analysis.arg_slots.iter().enumerate() {
                            let arg_heap_cell = arg_slots[slot_i];
                            match &slot_analysis.fate {
                                Fate::DirectTouch => {
                                    *self.fate(version, arg_heap_cell) = Fate::DirectTouch;
                                    for &other in
                                        &self.forward_state.heap_cells[arg_heap_cell].aliases
                                    {
                                        self.fate(version, other).union_with(&Fate::Other {
                                            indirect_touch: true,
                                            ret_slots: Set::new(),
                                        });
                                    }
                                }
                                Fate::Other {
                                    indirect_touch,
                                    ret_slots: callee_ret_slots,
                                } => {
                                    let fate = self.fate(version, arg_heap_cell);
                                    fate.union_with(&Fate::Other {
                                        indirect_touch: *indirect_touch,
                                        ret_slots: Set::new(),
                                    });
                                    for &callee_ret_slot in callee_ret_slots {
                                        self.copy_fate(
                                            version,
                                            ret_slots[callee_ret_slot as usize],
                                            arg_heap_cell,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }

                // We don't use 'replace_none' here because this value may be written multiple times
                // during fixed-point iteration.
                self.call_fates[callee_spec_var] = Some(CallFates {
                    arg_fates,
                    ret_fates,
                });
            }

            ir::OpKind::ConstRef { const_: _ } | ir::OpKind::NewHeapCell | ir::OpKind::EmptyBag => {
                debug_assert_eq!(input_slot_arrs.len(), 0);
            }

            ir::OpKind::UnknownWith | ir::OpKind::RecursiveTouch => {
                for input_slots in input_slot_arrs {
                    for &heap_cell in input_slots {
                        *self.fate(version, heap_cell) = Fate::DirectTouch;
                        for &other in &self.forward_state.heap_cells[heap_cell].aliases {
                            self.fate(version, other).union_with(&Fate::Other {
                                indirect_touch: true,
                                ret_slots: Set::new(),
                            });
                        }
                    }
                }
            }

            ir::OpKind::UpdateWriteOnly { update_mode_var } => {
                debug_assert_eq!(ret_slots.len(), 0);
                debug_assert_eq!(input_slot_arrs.len(), 1);
                debug_assert_eq!(input_slot_arrs[0].len(), 1);
                let heap_cell = input_slot_arrs[0][0];
                let fate = self.fate(version, heap_cell).clone();
                // Don't use replace_none here because we want to override update fate values during
                // fixed-point iteration
                self.update_fates[update_mode_var] = Some(fate);
            }

            ir::OpKind::BagInsert => {
                debug_assert_eq!(input_slot_arrs.len(), 2);
                for input_slots in input_slot_arrs {
                    debug_assert_eq!(input_slots.len(), ret_slots.len());
                    for (&input_heap_cell, &ret_heap_cell) in
                        input_slots.iter().zip(ret_slots.iter())
                    {
                        self.copy_fate(version, ret_heap_cell, input_heap_cell);
                    }
                }
            }

            ir::OpKind::BagGet => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                debug_assert_eq!(input_slot_arrs[0].len(), ret_slots.len());
                for (&input_heap_cell, &ret_heap_cell) in
                    input_slot_arrs[0].iter().zip(ret_slots.iter())
                {
                    self.copy_fate(version, ret_heap_cell, input_heap_cell);
                }
            }

            ir::OpKind::BagRemove => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                debug_assert_eq!(input_slot_arrs[0].len() * 2, ret_slots.len());
                for (&input_heap_cell, &ret_heap_cell) in input_slot_arrs[0]
                    .iter()
                    .chain(input_slot_arrs[0].iter())
                    .zip(ret_slots.iter())
                {
                    self.copy_fate(version, ret_heap_cell, input_heap_cell);
                }
            }

            ir::OpKind::MakeTuple => {
                for (&input_heap_cell, &ret_heap_cell) in input_slot_arrs
                    .iter()
                    .flat_map(|slots| slots.iter())
                    .zip(ret_slots.iter())
                {
                    self.copy_fate(version, ret_heap_cell, input_heap_cell);
                }
            }

            ir::OpKind::GetTupleField { field_idx } => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                let input_type = graph.values().node(val_node.inputs[0]).op.result_type;
                let field_slots = if let TypeSlots::Tuple { field_slots } = &sc.slots()[input_type]
                {
                    field_slots
                } else {
                    unreachable!()
                };
                let (start, end) = field_slots.sub_slots(*field_idx);
                debug_assert_eq!((end - start) as usize, ret_slots.len());
                for (&input_heap_cell, &ret_heap_cell) in input_slot_arrs[0]
                    [start as usize..end as usize]
                    .iter()
                    .zip(ret_slots.iter())
                {
                    self.copy_fate(version, ret_heap_cell, input_heap_cell);
                }
            }

            ir::OpKind::MakeUnion { variant_idx } => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                let variant_slots = if let TypeSlots::Union { variant_slots } =
                    &sc.slots()[val_node.op.result_type]
                {
                    variant_slots
                } else {
                    unreachable!()
                };
                let (start, end) = variant_slots.sub_slots(*variant_idx);
                debug_assert_eq!((end - start) as usize, input_slot_arrs[0].len());
                for (&input_heap_cell, &ret_heap_cell) in input_slot_arrs[0]
                    .iter()
                    .zip(ret_slots[start as usize..end as usize].iter())
                {
                    self.copy_fate(version, ret_heap_cell, input_heap_cell);
                }
            }

            ir::OpKind::UnwrapUnion { variant_idx } => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                let input_type = graph.values().node(val_node.inputs[0]).op.result_type;
                let variant_slots =
                    if let TypeSlots::Union { variant_slots } = &sc.slots()[input_type] {
                        variant_slots
                    } else {
                        unreachable!()
                    };
                let (start, end) = variant_slots.sub_slots(*variant_idx);
                debug_assert_eq!((end - start) as usize, ret_slots.len());
                for (&input_heap_cell, &ret_heap_cell) in input_slot_arrs[0]
                    [start as usize..end as usize]
                    .iter()
                    .zip(ret_slots.iter())
                {
                    self.copy_fate(version, ret_heap_cell, input_heap_cell);
                }
            }

            ir::OpKind::MakeNamed => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                debug_assert_eq!(ret_slots.len(), 1);
                for &input_heap_cell in input_slot_arrs[0] {
                    self.copy_fate(version, ret_slots[0], input_heap_cell);
                }
            }

            ir::OpKind::UnwrapNamed => {
                debug_assert_eq!(input_slot_arrs.len(), 1);
                debug_assert_eq!(input_slot_arrs[0].len(), 1);
                for &ret_heap_cell in ret_slots {
                    self.copy_fate(version, ret_heap_cell, input_slot_arrs[0][0]);
                }
            }
        }
    }

    fn analyze_block(
        &mut self,
        sc: &mut SlotCache,
        ctx: &mut SccAnalysisContext,
        graph: &ir::Graph,
        block: ir::BlockId,
        scc_kind: SccKind,
    ) {
        let info = graph.blocks().block_info(block);
        let parents = info
            .jump_targets
            .iter()
            .map(|&target| match target {
                ir::JumpTarget::Block(successor) => self.block_versions[successor].unwrap(),
                ir::JumpTarget::Ret => self.ret_version,
            })
            .collect();
        let block_version = self.versions.push(BackwardStateVersion {
            parents,
            overlay: HashMap::new(),
        });
        if let Some(target_arg_id) = &info.target_arg {
            let target_arg_slots = self.forward_state.value_slots[target_arg_id].unwrap();
            for target in &info.jump_targets {
                let parent_slots = match target {
                    ir::JumpTarget::Block(successor) => {
                        let successor_param_id =
                            graph.blocks().block_info(*successor).param.unwrap();
                        self.forward_state.value_slots[successor_param_id].unwrap()
                    }
                    ir::JumpTarget::Ret => self.ret_slots,
                };
                debug_assert_eq!(target_arg_slots.len(), parent_slots.len());
                for (&target_arg_heap_cell, &parent_param_heap_cell) in
                    target_arg_slots.iter().zip(parent_slots.iter())
                {
                    // TODO: Perform equivalent logic for entry argument cells
                    self.copy_fate(block_version, parent_param_heap_cell, target_arg_heap_cell);
                }
            }
        }
        match scc_kind {
            SccKind::Acyclic => {
                if cfg!(debug_assertions) {
                    for val_id in block_values_inclusive(graph, block) {
                        debug_assert!(self.forward_state.value_slots_inductive[val_id].is_none());
                    }
                }
            }
            SccKind::Cyclic => {
                for val_id in block_values_inclusive(graph, block) {
                    let inductive_slots = self.forward_state.value_slots_inductive[val_id].unwrap();
                    let main_slots = self.forward_state.value_slots[val_id].unwrap();
                    debug_assert_eq!(inductive_slots.len(), main_slots.len());
                    for (&inductive_heap_cell, &main_heap_cell) in
                        inductive_slots.iter().zip(main_slots.iter())
                    {
                        self.copy_fate(block_version, inductive_heap_cell, main_heap_cell);
                        if matches!(
                            self.fate(block_version, inductive_heap_cell),
                            Fate::DirectTouch
                        ) {
                            for &other in &self.forward_state.heap_cells[main_heap_cell].aliases {
                                self.fate(block_version, other).union_with(&Fate::Other {
                                    indirect_touch: true,
                                    ret_slots: Set::new(),
                                });
                            }
                        }
                    }
                }
            }
        }
        for val_id in graph.blocks().block_values_rev(block) {
            self.analyze_value(sc, ctx, block_version, graph, val_id);
        }
        replace_none(&mut self.block_versions[block], block_version).unwrap();
    }

    fn analyze_block_scc(
        &mut self,
        sc: &mut SlotCache,
        ctx: &mut SccAnalysisContext,
        graph: &ir::Graph,
        scc_id: ir::SccId,
    ) {
        let scc = graph.sccs().get(scc_id);
        match scc.info {
            SccKind::Acyclic => {
                debug_assert_eq!(scc.items.len(), 1);
                self.analyze_block(sc, ctx, graph, scc.items[0], SccKind::Acyclic);
            }
            SccKind::Cyclic => {
                // TODO: Don't heap allocate here on the fast path
                let scc_set: HashSet<_> = scc.items.iter().cloned().collect();
                let successor_versions: SmallVec<_> = scc
                    .items
                    .iter()
                    .flat_map(|&block| {
                        graph
                            .blocks()
                            .block_info(block)
                            .jump_targets
                            .iter()
                            .filter_map(|&target| match target {
                                ir::JumpTarget::Block(successor)
                                    if scc_set.contains(&successor) =>
                                {
                                    None
                                }
                                ir::JumpTarget::Block(successor) => {
                                    Some(self.block_versions[successor].unwrap())
                                }
                                ir::JumpTarget::Ret => Some(self.ret_version),
                            })
                    })
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .collect();
                let combined_parent = self.versions.push(BackwardStateVersion {
                    parents: successor_versions,
                    overlay: HashMap::new(),
                });
                let mut prev_iter_versions = HashMap::new();
                for block in scc.items {
                    prev_iter_versions.insert(block, combined_parent);
                    replace_none(&mut self.block_versions[block], combined_parent).unwrap();
                }
                // TODO: is this the correct set of heap cells to check for each block?
                // TODO: do we need to separately check update modes?
                let heap_cells_to_check = scc
                    .items
                    .iter()
                    .map(|&block| {
                        (
                            block,
                            block_values_inclusive(graph, block)
                                .flat_map(|val_id| {
                                    self.forward_state.value_slots[val_id]
                                        .unwrap()
                                        .iter()
                                        .chain(
                                            self.forward_state.value_slots_inductive[val_id]
                                                .unwrap()
                                                .iter(),
                                        )
                                        .cloned()
                                })
                                .collect::<HashSet<_>>(),
                        )
                    })
                    .collect::<HashMap<_, _>>();
                loop {
                    let mut curr_iter_versions = HashMap::new();
                    for &block in scc.items {
                        debug_assert!(self.block_versions[block].is_some());
                        self.block_versions[block] = None;
                        self.analyze_block(sc, ctx, graph, block, SccKind::Cyclic);
                        curr_iter_versions.insert(block, self.block_versions[block].unwrap());
                    }
                    let mut is_fixed_point = true;
                    'fixed_point_check_loop: for &block in scc.items {
                        let prev_version = prev_iter_versions[&block];
                        let curr_version = self.block_versions[block].unwrap();
                        debug_assert_eq!(curr_iter_versions[&block], curr_version);
                        for &heap_cell in &heap_cells_to_check[&block] {
                            debug_assert_ne!(prev_version, curr_version);
                            let prev_fate = std::mem::take(self.fate(prev_version, heap_cell));
                            let curr_fate = self.fate(curr_version, heap_cell);
                            let is_same = prev_fate == *curr_fate;
                            *self.fate(prev_version, heap_cell) = prev_fate;
                            if !is_same {
                                is_fixed_point = false;
                                break 'fixed_point_check_loop;
                            }
                        }
                    }
                    if is_fixed_point {
                        break;
                    }
                }
            }
        }
    }

    fn analyze_graph(
        forward_state: &'b ForwardState<'a>,
        ret_slots: &'a [HeapCellId],
        sc: &mut SlotCache,
        ctx: &mut SccAnalysisContext,
        graph: &ir::Graph,
    ) -> (Self, StateVersionId) {
        let mut versions = IdVec::new();
        let ret_version = versions.push(BackwardStateVersion {
            parents: SmallVec::new(),
            overlay: ret_slots
                .iter()
                .enumerate()
                .map(|(slot_i, &heap_cell)| {
                    (
                        heap_cell,
                        Fate::Other {
                            indirect_touch: false,
                            ret_slots: std::iter::once(slot_i.try_into().unwrap()).collect(),
                        },
                    )
                })
                .collect(),
        });
        let mut state = Self {
            forward_state,
            versions,
            ret_version,
            ret_slots,
            block_versions: IdVec::filled_with(graph.blocks().block_count(), || None),
            update_fates: IdVec::filled_with(graph.update_mode_vars(), || None),
            call_fates: IdVec::filled_with(graph.callee_spec_vars(), || None),
        };
        for scc_id in graph.sccs().count().iter().rev() {
            state.analyze_block_scc(sc, ctx, graph, scc_id);
        }
        let entry_version = state.block_versions[graph.entry_block()].unwrap();
        if let Some(arg_slots) = forward_state.arg_slots {
            let entry_param_id = graph
                .blocks()
                .block_info(graph.entry_block())
                .param
                .unwrap();
            let entry_param_slots = forward_state.value_slots[entry_param_id].unwrap();
            debug_assert_eq!(arg_slots.len(), entry_param_slots.len());
            for (&arg_heap_cell, &entry_param_heap_cell) in
                arg_slots.iter().zip(entry_param_slots.iter())
            {
                state.copy_fate(entry_version, entry_param_heap_cell, arg_heap_cell);
            }
        }
        (state, entry_version)
    }
}

fn analyze_func(
    sc: &mut SlotCache,
    ctx: &mut SccAnalysisContext,
    func_def: &ir::FuncDef,
    arg_alias: Option<NormPair<u32>>,
) -> FuncAnalysis {
    let slots_arena = Arena::new();
    let (forward, ret_slots) =
        ForwardState::analyze_graph(&slots_arena, sc, ctx, &func_def.graph, arg_alias);
    let (mut backward, entry_version) =
        BackwardState::analyze_graph(&forward, ret_slots, sc, ctx, &func_def.graph);
    let mut heap_cell_to_arg_slot = HashMap::<HeapCellId, u32>::new();
    for (slot_i, &heap_cell) in forward.arg_slots.unwrap().iter().enumerate() {
        let existing = heap_cell_to_arg_slot.insert(heap_cell, slot_i.try_into().unwrap());
        debug_assert!(existing.is_none());
    }
    let mut heap_cell_to_ret_slots = HashMap::<HeapCellId, SmallVec<[u32; 4]>>::new();
    for (slot_i, &heap_cell) in ret_slots.iter().enumerate() {
        heap_cell_to_ret_slots
            .entry(heap_cell)
            .or_insert_with(SmallVec::new)
            .push(slot_i.try_into().unwrap());
    }
    let arg_slot_analyses = forward
        .arg_slots
        .unwrap()
        .iter()
        .map(|&heap_cell| ArgSlotAnalysis {
            fate: backward.fate(entry_version, heap_cell).clone(),
        })
        .collect();
    let ret_slot_analyses = ret_slots
        .iter()
        .enumerate()
        .map(|(this_ret_slot_i, &heap_cell)| {
            let mut arg_aliases = Set::new();
            let mut ret_aliases = Set::new();
            for other in std::iter::once(heap_cell)
                .chain(forward.heap_cells[heap_cell].aliases.iter().cloned())
            {
                if let Some(&arg_slot_i) = heap_cell_to_arg_slot.get(&other) {
                    arg_aliases.insert(arg_slot_i);
                }
                if let Some(ret_slots_i) = heap_cell_to_ret_slots.get(&other) {
                    for &ret_slot_i in ret_slots_i {
                        if ret_slot_i as usize != this_ret_slot_i {
                            ret_aliases.insert(ret_slot_i);
                        }
                    }
                }
            }
            RetSlotAnalysis {
                from_const: matches!(forward.heap_cells[heap_cell].origin, Origin::FromConst),
                arg_aliases,
                ret_aliases,
            }
        })
        .collect();
    FuncAnalysis {
        graph_analysis: GraphAnalysis {
            updates: IdVec::filled_with_indexed(
                func_def.graph.update_mode_vars(),
                |update_mode_var| UpdateAnalysis {
                    origin: forward.update_origins[update_mode_var]
                        .as_ref()
                        .unwrap()
                        .clone(),
                    fate: backward.update_fates[update_mode_var]
                        .as_ref()
                        .unwrap()
                        .clone(),
                },
            ),
            calls: IdVec::filled_with_indexed(
                func_def.graph.callee_spec_vars(),
                |callee_spec_var| {
                    let call_fates = backward.call_fates[callee_spec_var].as_ref().unwrap();
                    CallAnalysis {
                        callee: forward.calls[callee_spec_var].unwrap(),
                        arg_aliases: forward.call_arg_aliases[callee_spec_var]
                            .as_ref()
                            .unwrap()
                            .clone(),
                        arg_slots: forward.call_arg_origins[callee_spec_var]
                            .as_ref()
                            .unwrap()
                            .iter()
                            .zip(call_fates.arg_fates.iter())
                            .map(|(origin, fate)| ArgAnalysis {
                                origin: origin.clone(),
                                fate: fate.clone(),
                            })
                            .collect(),
                        ret_slots: call_fates.ret_fates.clone(),
                    }
                },
            ),
        },
        arg_slots: arg_slot_analyses,
        ret_slots: ret_slot_analyses,
    }
}

id_type! {
    FuncSccId(u32);
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct UpdateAnalysis {
    origin: Origin,
    fate: Fate,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ArgAnalysis {
    origin: Origin,
    fate: Fate,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct CallAnalysis {
    // Find a better place to store the callee
    callee: FuncId,
    arg_aliases: Set<NormPair<u32>>,
    arg_slots: SmallVec<[ArgAnalysis; 4]>,
    ret_slots: SmallVec<[Fate; 4]>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct GraphAnalysis {
    updates: IdVec<api::UpdateModeVarId, UpdateAnalysis>,
    calls: IdVec<api::CalleeSpecVarId, CallAnalysis>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ArgSlotAnalysis {
    fate: Fate,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RetSlotAnalysis {
    from_const: bool,
    arg_aliases: Set<u32>,
    ret_aliases: Set<u32>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct FuncAnalysis {
    graph_analysis: GraphAnalysis,
    arg_slots: SmallVec<[ArgSlotAnalysis; 4]>,
    ret_slots: SmallVec<[RetSlotAnalysis; 4]>,
}

#[derive(Clone, Debug)]
struct GlobalAnalysisContext<'a> {
    func_defs: &'a IdVec<FuncId, ir::FuncDef>,
    sccs: &'a FlatSlices<FuncSccId, SccKind, FuncId>,
    func_to_scc: &'a IdVec<FuncId, FuncSccId>,
    committed: IdVec<FuncId, HashMap<Option<NormPair<u32>>, FuncAnalysis>>,
}

impl<'a> GlobalAnalysisContext<'a> {
    fn analyze(
        &mut self,
        sc: &mut SlotCache,
        func: FuncId,
        arg_alias: Option<NormPair<u32>>,
    ) -> &FuncAnalysis {
        debug_assert!(!self.committed[func].contains_key(&arg_alias));
        let scc = self.func_to_scc[func];
        let scc_kind = *self.sccs.get(scc).info;
        let mut scc_ctx = SccAnalysisContext {
            global: &mut *self,
            scc,
            prev_iter: HashMap::new(),
            curr_iter: HashMap::new(),
        };
        match scc_kind {
            SccKind::Acyclic => {
                scc_ctx.get_analysis(sc, func, arg_alias);
                debug_assert_eq!(scc_ctx.curr_iter.len(), 1);
            }
            SccKind::Cyclic => loop {
                scc_ctx.get_analysis(sc, func, arg_alias);
                // TODO: only compare "signature" information here, not internal annotations on body
                // values.
                if scc_ctx.curr_iter == scc_ctx.prev_iter {
                    break;
                }
                scc_ctx.prev_iter = std::mem::take(&mut scc_ctx.curr_iter);
            },
        };
        let results = scc_ctx.curr_iter;
        for ((analyzed_func, analyzed_arg_alias), analysis) in results {
            let existing = self.committed[analyzed_func]
                .insert(analyzed_arg_alias, analysis.unwrap_complete());
            debug_assert!(existing.is_none());
        }
        &self.committed[func][&arg_alias]
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq)]
enum AnalysisState {
    Pending,
    Complete(FuncAnalysis),
}

impl AnalysisState {
    fn unwrap_complete(self) -> FuncAnalysis {
        match self {
            AnalysisState::Pending => unreachable!(),
            AnalysisState::Complete(analysis) => analysis,
        }
    }

    fn unwrap_complete_ref(&self) -> &FuncAnalysis {
        match self {
            AnalysisState::Pending => unreachable!(),
            AnalysisState::Complete(analysis) => analysis,
        }
    }
}

#[derive(Debug)]
struct SccAnalysisContext<'a, 'b> {
    global: &'b mut GlobalAnalysisContext<'a>,
    scc: FuncSccId,
    // Invariant: 'prev_iter' should contain no 'Pending' analyses
    prev_iter: HashMap<(FuncId, Option<NormPair<u32>>), AnalysisState>,
    curr_iter: HashMap<(FuncId, Option<NormPair<u32>>), AnalysisState>,
}

impl<'a, 'b> SccAnalysisContext<'a, 'b> {
    fn get_analysis<'c>(
        &'c mut self,
        sc: &mut SlotCache,
        func: FuncId,
        arg_alias: Option<NormPair<u32>>,
    ) -> Option<&'c FuncAnalysis> {
        if self.global.committed[func].contains_key(&arg_alias) {
            // TODO: is there a way to avoid the double lookup here while passing the borrow
            // checker?
            return Some(&self.global.committed[func][&arg_alias]);
        }
        if self.global.func_to_scc[func] != self.scc {
            return Some(self.global.analyze(sc, func, arg_alias));
        }
        // TODO: can we resolve this clippy error while passing the borrow checker?
        #[allow(clippy::map_entry)]
        if self.curr_iter.contains_key(&(func, arg_alias)) {
            // TODO: as above, can we avoid the double lookup?
            match &self.curr_iter[&(func, arg_alias)] {
                AnalysisState::Complete(analysis) => Some(analysis),
                AnalysisState::Pending => self
                    .prev_iter
                    .get(&(func, arg_alias))
                    .map(AnalysisState::unwrap_complete_ref),
            }
        } else {
            self.curr_iter
                .insert((func, arg_alias), AnalysisState::Pending);
            let analysis = analyze_func(sc, self, &self.global.func_defs[func], arg_alias);
            match self.curr_iter.entry((func, arg_alias)) {
                std::collections::hash_map::Entry::Occupied(mut occupied) => {
                    *occupied.get_mut() = AnalysisState::Complete(analysis);
                    Some(occupied.into_mut().unwrap_complete_ref())
                }
                std::collections::hash_map::Entry::Vacant(_) => {
                    unreachable!()
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Query {
    // TODO: improve sparsity of contextual information, prune everything inessential
    arg_aliases: BTreeSet<NormPair<u32>>,
    // For the purposes of `arg_slots_touched`, an arg slot being 'FromConst' is the same as being
    // touched after the call.
    arg_slots_touched: SmallVec<[bool; 8]>,
    ret_slots_touched: SmallVec<[bool; 8]>,
}

impl Query {
    fn to_spec(&self, func: FuncId) -> api::FuncSpec {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(func.0.to_le_bytes());
        hasher.update((self.arg_aliases.len() as u64).to_le_bytes());
        for arg_alias in &self.arg_aliases {
            hasher.update(arg_alias.fst().to_le_bytes());
            hasher.update(arg_alias.snd().to_le_bytes());
        }
        hasher.update((self.arg_slots_touched.len() as u64).to_le_bytes());
        for &arg_touched in &self.arg_slots_touched {
            hasher.update(&[arg_touched as u8]);
        }
        hasher.update((self.ret_slots_touched.len() as u64).to_le_bytes());
        for &ret_touched in &self.ret_slots_touched {
            hasher.update(&[ret_touched as u8]);
        }
        api::FuncSpec(hasher.finalize().into())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct FuncSolution {
    pub(crate) update_modes: IdVec<api::UpdateModeVarId, api::UpdateMode>,
    pub(crate) callee_specs: IdVec<api::CalleeSpecVarId, api::FuncSpec>,
}

#[derive(Clone, Debug)]
pub(crate) struct FuncSolutions {
    pub(crate) solutions: IdVec<FuncId, HashMap<api::FuncSpec, Option<FuncSolution>>>,
}

fn resolve_origin<'a>(query: &Query, mut origins: impl Iterator<Item = &'a Origin>) -> bool {
    origins.any(|origin| match origin {
        Origin::FromConst => true,
        Origin::FromArgSlots(arg_slots) => arg_slots
            .iter()
            .any(|&arg_slot| query.arg_slots_touched[arg_slot as usize]),
    })
}

fn resolve_fate<'a>(query: &Query, mut fates: impl Iterator<Item = &'a Fate>) -> bool {
    fates.any(|fate| match fate {
        Fate::DirectTouch => true,
        Fate::Other {
            indirect_touch,
            ret_slots,
        } => {
            *indirect_touch
                || ret_slots
                    .iter()
                    .any(|&ret_slot| query.ret_slots_touched[ret_slot as usize])
        }
    })
}

impl FuncSolutions {
    fn resolve(
        &mut self,
        analyses: &IdVec<FuncId, HashMap<Option<NormPair<u32>>, FuncAnalysis>>,
        func: FuncId,
        query: &Query,
    ) -> api::FuncSpec {
        let spec = query.to_spec(func);
        if let std::collections::hash_map::Entry::Vacant(vacant) = self.solutions[func].entry(spec)
        {
            let func_analyses = &analyses[func];
            let basic_analysis = &func_analyses[&None].graph_analysis;
            vacant.insert(None);
            let query_analyses: SmallVec<[&GraphAnalysis; 8]> = std::iter::once(basic_analysis)
                .chain(
                    query
                        .arg_aliases
                        .iter()
                        .map(|&arg_alias| &func_analyses[&Some(arg_alias)].graph_analysis),
                )
                .collect();
            let update_modes =
                IdVec::filled_with_indexed(basic_analysis.updates.count(), |update_mode_var| {
                    let touched = resolve_origin(
                        query,
                        query_analyses
                            .iter()
                            .map(|analysis| &analysis.updates[update_mode_var].origin),
                    ) || resolve_fate(
                        query,
                        query_analyses
                            .iter()
                            .map(|analysis| &analysis.updates[update_mode_var].fate),
                    );
                    if touched {
                        api::UpdateMode::Immutable
                    } else {
                        api::UpdateMode::InPlace
                    }
                });
            let callee_specs =
                IdVec::filled_with_indexed(basic_analysis.calls.count(), |callee_spec_var| {
                    let mut sub_arg_aliases = BTreeSet::new();
                    for analysis in &query_analyses {
                        sub_arg_aliases.extend(&analysis.calls[callee_spec_var].arg_aliases);
                    }
                    let num_arg_slots = basic_analysis.calls[callee_spec_var].arg_slots.len();
                    let num_ret_slots = basic_analysis.calls[callee_spec_var].ret_slots.len();
                    let sub_arg_slots_touched = (0..num_arg_slots)
                        .map(|arg_slot_i| {
                            resolve_origin(
                                query,
                                query_analyses.iter().map(|analysis| {
                                    &analysis.calls[callee_spec_var].arg_slots[arg_slot_i].origin
                                }),
                            ) || resolve_fate(
                                query,
                                query_analyses.iter().map(|analysis| {
                                    &analysis.calls[callee_spec_var].arg_slots[arg_slot_i].fate
                                }),
                            )
                        })
                        .collect();
                    let sub_ret_slots_touched = (0..num_ret_slots)
                        .map(|ret_slot_i| {
                            resolve_fate(
                                query,
                                query_analyses.iter().map(|analysis| {
                                    &analysis.calls[callee_spec_var].ret_slots[ret_slot_i]
                                }),
                            )
                        })
                        .collect();
                    let sub_query = Query {
                        arg_aliases: sub_arg_aliases,
                        arg_slots_touched: sub_arg_slots_touched,
                        ret_slots_touched: sub_ret_slots_touched,
                    };
                    self.resolve(
                        analyses,
                        basic_analysis.calls[callee_spec_var].callee,
                        &sub_query,
                    )
                });
            let solution = FuncSolution {
                update_modes,
                callee_specs,
            };
            self.solutions[func].insert(spec, Some(solution));
        }
        spec
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ProgramSolutions {
    pub(crate) funcs: FuncSolutions,
    pub(crate) entry_points: IdVec<EntryPointId, api::FuncSpec>,
}

pub(crate) fn analyze(tc: TypeCache, program: &ir::Program) -> ProgramSolutions {
    let mut sc = SlotCache::new(tc);

    let func_sccs: FlatSlices<FuncSccId, _, _> =
        strongly_connected(program.funcs.count(), |func_id| {
            let func_def = &program.funcs[func_id];
            let values = func_def.graph.values();
            values
                .count()
                .iter()
                .filter_map(move |val_id| match &values.node(val_id).op.kind {
                    ir::ValueKind::Op(ir::OpKind::Call {
                        callee,
                        callee_spec_var: _,
                    }) => Some(*callee),

                    _ => None,
                })
        });

    let mut func_to_scc = IdVec::filled_with(program.funcs.count(), || FuncSccId(u32::MAX));
    for scc_id in func_sccs.count().iter() {
        for &func in func_sccs.get(scc_id).items {
            func_to_scc[func] = scc_id;
        }
    }

    let mut ctx = GlobalAnalysisContext {
        func_defs: &program.funcs,
        sccs: &func_sccs,
        func_to_scc: &func_to_scc,
        committed: IdVec::filled_with(program.funcs.count(), HashMap::new),
    };

    for (_, &func) in &program.entry_points {
        if !ctx.committed[func].contains_key(&None) {
            ctx.analyze(&mut sc, func, None);
        }
    }

    let mut func_solutions = FuncSolutions {
        solutions: IdVec::filled_with(program.funcs.count(), HashMap::new),
    };

    let entry_point_solutions = program.entry_points.map(|_, &func| {
        func_solutions.resolve(
            &ctx.committed,
            func,
            &Query {
                arg_aliases: BTreeSet::new(),
                arg_slots_touched: SmallVec::new(),
                ret_slots_touched: SmallVec::new(),
            },
        )
    });

    ProgramSolutions {
        funcs: func_solutions,
        entry_points: entry_point_solutions,
    }
}
