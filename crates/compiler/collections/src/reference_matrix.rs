type Order = bitvec::order::Lsb0;
type Element = usize;
type BitVec = bitvec::vec::BitVec<Element, Order>;
type BitSlice = bitvec::prelude::BitSlice<Element, Order>;

/// A square boolean matrix used to store relations
///
/// We use this for sorting definitions so every definition is defined before it is used.
/// This functionality is also used to spot and report invalid recursion.
#[derive(Debug)]
pub struct ReferenceMatrix {
    bitvec: BitVec,
    length: usize,
}

impl ReferenceMatrix {
    pub fn new(length: usize) -> Self {
        Self {
            bitvec: BitVec::repeat(false, length * length),
            length,
        }
    }

    pub fn references_for(&self, row: usize) -> impl Iterator<Item = usize> + '_ {
        self.row_slice(row).iter_ones()
    }

    #[inline(always)]
    fn row_slice(&self, row: usize) -> &BitSlice {
        &self.bitvec[row * self.length..][..self.length]
    }

    #[inline(always)]
    pub fn set_row_col(&mut self, row: usize, col: usize, value: bool) {
        self.bitvec.set(row * self.length + col, value)
    }

    #[inline(always)]
    pub fn get_row_col(&self, row: usize, col: usize) -> bool {
        self.bitvec[row * self.length + col]
    }
}

// Topological sort and strongly-connected components
//
// Adapted from the Pathfinding crate v2.0.3 by Samuel Tardieu <sam@rfc1149.net>,
// licensed under the Apache License, version 2.0 - https://www.apache.org/licenses/LICENSE-2.0
//
// The original source code can be found at: https://github.com/samueltardieu/pathfinding
//
// Thank you, Samuel!
impl ReferenceMatrix {
    pub fn topological_sort_into_groups(&self) -> TopologicalSort {
        if self.length == 0 {
            return TopologicalSort::Groups { groups: Vec::new() };
        }

        let mut preds_map: Vec<i64> = vec![0; self.length];

        // this is basically summing the columns, I don't see a better way to do it
        for row in self.bitvec.chunks_exact(self.length) {
            for succ in row.iter_ones() {
                preds_map[succ] += 1;
            }
        }

        let mut groups = Vec::<Vec<u32>>::new();

        // the initial group contains all symbols with no predecessors
        let mut prev_group: Vec<u32> = preds_map
            .iter()
            .enumerate()
            .filter_map(|(node, &num_preds)| {
                if num_preds == 0 {
                    Some(node as u32)
                } else {
                    None
                }
            })
            .collect();

        if prev_group.is_empty() {
            let remaining: Vec<u32> = (0u32..self.length as u32).collect();

            return TopologicalSort::HasCycles {
                groups: Vec::new(),
                nodes_in_cycle: remaining,
            };
        }

        while preds_map.iter().any(|x| *x > 0) {
            let mut next_group = Vec::<u32>::new();
            for node in &prev_group {
                for succ in self.references_for(*node as usize) {
                    {
                        let num_preds = preds_map.get_mut(succ).unwrap();
                        *num_preds = num_preds.saturating_sub(1);
                        if *num_preds > 0 {
                            continue;
                        }
                    }

                    // NOTE: we use -1 to mark nodes that have no predecessors, but are already
                    // part of an earlier group. That ensures nodes are added to just 1 group
                    let count = preds_map[succ];
                    preds_map[succ] = -1;

                    if count > -1 {
                        next_group.push(succ as u32);
                    }
                }
            }
            groups.push(std::mem::replace(&mut prev_group, next_group));
            if prev_group.is_empty() {
                let remaining: Vec<u32> = (0u32..self.length as u32)
                    .filter(|i| preds_map[*i as usize] > 0)
                    .collect();

                return TopologicalSort::HasCycles {
                    groups,
                    nodes_in_cycle: remaining,
                };
            }
        }
        groups.push(prev_group);

        TopologicalSort::Groups { groups }
    }

    /// Get the strongly-connected components all nodes in the matrix
    pub fn strongly_connected_components_all(&self) -> Sccs {
        let bitvec = BitVec::repeat(true, self.length);
        self.strongly_connected_components_subset(&bitvec)
    }

    /// Get the strongly-connected components of a set of input nodes.
    pub fn strongly_connected_components_subset(&self, nodes: &BitSlice) -> Sccs {
        let mut params = Params::new(self.length, nodes);

        'outer: loop {
            for (node, value) in params.preorders.iter().enumerate() {
                if let Preorder::Removed = value {
                    continue;
                }

                recurse_onto(self.length, &self.bitvec, node, &mut params);

                continue 'outer;
            }

            break params.scc;
        }
    }
}

#[derive(Debug)]
pub enum TopologicalSort {
    /// There were no cycles, all nodes have been partitioned into groups
    Groups { groups: Vec<Vec<u32>> },
    /// Cycles were found. All nodes that are not part of a cycle have been partitioned
    /// into groups. The other elements are in the `cyclic` vector. However, there may be
    /// many cycles, or just one big one. Use strongly-connected components to find out
    /// exactly what the cycles are and how they fit into the groups.
    HasCycles {
        groups: Vec<Vec<u32>>,
        nodes_in_cycle: Vec<u32>,
    },
}

#[derive(Clone, Copy)]
enum Preorder {
    Empty,
    Filled(usize),
    Removed,
}

struct Params {
    preorders: Vec<Preorder>,
    c: usize,
    p: Vec<u32>,
    s: Vec<u32>,
    scc: Sccs,
    scca: Vec<u32>,
}

impl Params {
    fn new(length: usize, group: &BitSlice) -> Self {
        let mut preorders = vec![Preorder::Removed; length];

        for index in group.iter_ones() {
            preorders[index] = Preorder::Empty;
        }

        Self {
            preorders,
            c: 0,
            s: Vec::new(),
            p: Vec::new(),
            scc: Sccs {
                matrix: ReferenceMatrix::new(length),
                components: 0,
                not_initial: BitVec::repeat(false, length),
            },
            // use u32::MAX as the sentinel empty value
            scca: vec![u32::MAX; length],
        }
    }
}

fn recurse_onto(length: usize, bitvec: &BitVec, v: usize, params: &mut Params) {
    params.preorders[v] = Preorder::Filled(params.c);

    params.c += 1;

    params.s.push(v as u32);
    params.p.push(v as u32);

    for w in bitvec[v * length..][..length].iter_ones() {
        if params.scca[w] == u32::MAX {
            match params.preorders[w] {
                Preorder::Filled(pw) => loop {
                    let index = *params.p.last().unwrap();

                    match params.preorders[index as usize] {
                        Preorder::Empty => unreachable!(),
                        Preorder::Filled(current) => {
                            if current > pw {
                                params.p.pop();
                            } else {
                                break;
                            }
                        }
                        Preorder::Removed => {}
                    }
                },
                Preorder::Empty => recurse_onto(length, bitvec, w, params),
                Preorder::Removed => {}
            }
        } else {
            params.scc.not_initial.set(params.scca[w] as _, true);
        }
    }

    if params.p.last() == Some(&(v as u32)) {
        params.p.pop();

        while let Some(node) = params.s.pop() {
            params
                .scc
                .matrix
                .set_row_col(params.scc.components, node as usize, true);
            params.scca[node as usize] = params.scc.components as _;
            params.preorders[node as usize] = Preorder::Removed;
            if node as usize == v {
                break;
            }
        }

        if !params.s.is_empty() {
            params.scc.not_initial.set(params.scc.components, true);
        }

        params.scc.components += 1;
    }
}

#[derive(Debug)]
pub struct Sccs {
    components: usize,
    matrix: ReferenceMatrix,
    not_initial: BitVec,
}

impl Sccs {
    /// Iterate over the individual components. Each component is represented as a bit vector where
    /// a one indicates that the node is part of the group and a zero that it is not.
    ///
    /// A good way to get the actual nodes is the `.iter_ones()` method.
    ///
    /// It is guaranteed that a group is non-empty, and that flattening the groups gives a valid
    /// topological ordering.
    pub fn groups(&self) -> impl DoubleEndedIterator<Item = (&'_ BitSlice, bool)> {
        // work around a panic when requesting a chunk size of 0
        let length = if self.matrix.length == 0 {
            // the `.take(self.components)` ensures the resulting iterator will be empty
            assert!(self.components == 0);

            1
        } else {
            self.matrix.length
        };

        self.matrix
            .bitvec
            .chunks_exact(length)
            .take(self.components)
            .enumerate()
            .map(|(c, slice)| (slice, !self.not_initial[c]))
    }

    /// Reorder the input slice based on the SCCs. This produces a topological sort
    pub fn reorder<T>(&self, slice: &mut [T]) {
        debug_assert_eq!(self.matrix.length, slice.len());

        let mut indices: Vec<_> = self.groups().flat_map(|(s, _)| s.iter_ones()).collect();

        for i in 0..slice.len() {
            let mut index = indices[i];
            while index < i {
                index = indices[index];
            }

            if i != index {
                indices[i] = index;
                slice.swap(i, index);
            }
        }
    }
}
