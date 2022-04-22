// see if we get better performance with different integer types
pub(crate) type Element = usize;
pub(crate) type BitVec = bitvec::vec::BitVec<Element>;
pub(crate) type BitSlice = bitvec::prelude::BitSlice<Element>;

/// A square boolean matrix used to store relations
///
/// We use this for sorting definitions so every definition is defined before it is used.
/// This functionality is also used to spot and report invalid recursion.
#[derive(Debug)]
pub(crate) struct ReferenceMatrix {
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
    pub fn get(&self, index: usize) -> bool {
        self.bitvec[index]
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
    #[allow(clippy::type_complexity)]
    pub fn topological_sort_into_groups(&self) -> Result<Vec<Vec<u32>>, (Vec<Vec<u32>>, Vec<u32>)> {
        let length = self.length;
        let bitvec = &self.bitvec;

        if length == 0 {
            return Ok(Vec::new());
        }

        let mut preds_map: Vec<i64> = vec![0; length];

        // this is basically summing the columns, I don't see a better way to do it
        for row in bitvec.chunks(length) {
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
            let remaining: Vec<u32> = (0u32..length as u32).collect();
            return Err((Vec::new(), remaining));
        }

        while preds_map.iter().any(|x| *x > 0) {
            let mut next_group = Vec::<u32>::new();
            for node in &prev_group {
                let row = &bitvec[length * (*node as usize)..][..length];
                for succ in row.iter_ones() {
                    {
                        let num_preds = preds_map.get_mut(succ).unwrap();
                        *num_preds = num_preds.saturating_sub(1);
                        if *num_preds > 0 {
                            continue;
                        }
                    }

                    let count = preds_map[succ];
                    preds_map[succ] = -1;

                    if count > -1 {
                        next_group.push(succ as u32);
                    }
                }
            }
            groups.push(std::mem::replace(&mut prev_group, next_group));
            if prev_group.is_empty() {
                let remaining: Vec<u32> = (0u32..length as u32)
                    .filter(|i| preds_map[*i as usize] > 0)
                    .collect();
                return Err((groups, remaining));
            }
        }
        groups.push(prev_group);

        Ok(groups)
    }

    pub fn strongly_connected_components(&self, group: &[u32]) -> Vec<Vec<u32>> {
        let mut params = Params::new(self.length, group);

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
    scc: Vec<Vec<u32>>,
    scca: Vec<u32>,
}

impl Params {
    fn new(length: usize, group: &[u32]) -> Self {
        let mut preorders = vec![Preorder::Removed; length];

        for value in group {
            preorders[*value as usize] = Preorder::Empty;
        }

        Self {
            preorders,
            c: 0,
            s: Vec::new(),
            p: Vec::new(),
            scc: Vec::new(),
            scca: Vec::new(),
        }
    }
}

fn recurse_onto(length: usize, bitvec: &BitVec, v: usize, params: &mut Params) {
    params.preorders[v] = Preorder::Filled(params.c);

    params.c += 1;

    params.s.push(v as u32);
    params.p.push(v as u32);

    for w in bitvec[v * length..][..length].iter_ones() {
        if !params.scca.contains(&(w as u32)) {
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
        }
    }

    if params.p.last() == Some(&(v as u32)) {
        params.p.pop();

        let mut component = Vec::new();
        while let Some(node) = params.s.pop() {
            component.push(node);
            params.scca.push(node);
            params.preorders[node as usize] = Preorder::Removed;
            if node as usize == v {
                break;
            }
        }
        params.scc.push(component);
    }
}
