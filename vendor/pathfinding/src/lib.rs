// Adapted from the Pathfinding crate v2.0.3 by Samuel Tardieu <sam@rfc1149.net>,
// licensed under the Apache License, version 2.0 - https://www.apache.org/licenses/LICENSE-2.0
//
// The original source code can be found at: https://github.com/samueltardieu/pathfinding
//
// Thank you, Samuel!
//
//
//
// This is modified from the original source to use the Roc compiler's preferred hashers
// instead of the SipHash hasher which Rust hash collections use by default.
//
// SipHash defends against hash flooding attacks by generating a random seed
// whenever a new hasher is instantiated, and which is designed to prevent attackers
// from crafting intentional collisions that amplify denial-of-service attacks.
// Since this is a compiler, we aren't worried about denial-of-service attacks.
//
// The primary motivation for this change is wanting the compiler to always give exactly
// the same answer given the same inputs. So if you give it the same source files, it should
// produce identical binaries every time. SipHash by design gives different answers on each run.
//
// Secondarily, SipHash isn't the fastest hashing algorithm out there, so we can get
// slightly better performance by using a faster hasher.

use roc_collections::all::{default_hasher, BuildHasher, MutSet};
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::mem;

/// Find a topological order in a directed graph if one exists.
///
/// - `nodes` is a collection of nodes.
/// - `successors` returns a list of successors for a given node.
///
/// The function returns either `Ok` with an acceptable topological order,
/// or `Err` with a node belonging to a cycle. In the latter case, the
/// strongly connected set can then be found using the
/// [`strongly_connected_component`](super::strongly_connected_components::strongly_connected_component)
/// function, or if only one of the loops is needed the [`bfs_loop`][super::bfs::bfs_loop] function
/// can be used instead to identify one of the shortest loops involving this node.
///
/// # Examples
///
/// We will sort integers from 1 to 9, each integer having its two immediate
/// greater numbers as successors:
///
/// //```
/// extern crate roc;
///
/// use roc::graph::topological_sort;
///
/// fn successors(node: &usize) -> Vec<usize> {
///   match *node {
///     n if n <= 7 => vec![n+1, n+2],
///     8 => vec![9],
///     _ => vec![],
///   }
/// }
///
/// let sorted = topological_sort(&[3, 7, 1, 4, 2, 9, 8, 6, 5], successors);
/// assert_eq!(sorted, Ok(vec![1, 2, 3, 4, 5, 6, 7, 8, 9]));
/// //```
///
/// If, however, there is a loop in the graph (for example, all nodes but 7
/// have also 7 has a successor), one of the nodes in the loop will be returned as
/// an error:
///
/// //```
/// extern crate roc;
///
/// use roc::graph::*;
///
/// fn successors(node: &usize) -> Vec<usize> {
///   match *node {
///     n if n <= 6 => vec![n+1, n+2, 7],
///     7 => vec![8, 9],
///     8 => vec![7, 9],
///     _ => vec![7],
///   }
/// }
///
/// let sorted = topological_sort(&[3, 7, 1, 4, 2, 9, 8, 6, 5], successors);
/// assert!(sorted.is_err());
///
/// // Let's assume that the returned node is 8 (it can be any node which is part
/// // of a loop). We can lookup up one of the shortest loops containing 8
/// // (8 -> 7 -> 8 is the unique loop with two hops containing 8):
///
/// // assert_eq!(bfs_loop(&8, successors), Some(vec![8, 7, 8]));
///
/// // We can also request the whole strongly connected set containing 8. Here
/// // 7, 8, and 9 are all reachable from one another.
///
/// let mut set = strongly_connected_component(&8, successors);
/// set.sort();
/// assert_eq!(set, vec![7, 8, 9]);
/// //```
pub fn topological_sort<N, FN, IN>(nodes: &[N], mut successors: FN) -> Result<Vec<N>, N>
where
    N: Eq + Hash + Clone,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = N>,
{
    let mut unmarked: MutSet<N> = nodes.iter().cloned().collect::<MutSet<_>>();
    let mut marked = HashSet::with_capacity_and_hasher(nodes.len(), default_hasher());
    let mut temp = MutSet::default();
    let mut sorted = VecDeque::with_capacity(nodes.len());
    while let Some(node) = unmarked.iter().next().cloned() {
        temp.clear();
        visit(
            &node,
            &mut successors,
            &mut unmarked,
            &mut marked,
            &mut temp,
            &mut sorted,
        )?;
    }
    Ok(sorted.into_iter().collect())
}

fn visit<N, FN, IN>(
    node: &N,
    successors: &mut FN,
    unmarked: &mut MutSet<N>,
    marked: &mut MutSet<N>,
    temp: &mut MutSet<N>,
    sorted: &mut VecDeque<N>,
) -> Result<(), N>
where
    N: Eq + Hash + Clone,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = N>,
{
    unmarked.remove(node);
    if marked.contains(node) {
        return Ok(());
    }
    if temp.contains(node) {
        return Err(node.clone());
    }
    temp.insert(node.clone());
    for n in successors(node) {
        visit(&n, successors, unmarked, marked, temp, sorted)?;
    }
    marked.insert(node.clone());
    sorted.push_front(node.clone());
    Ok(())
}

/// Topologically sort a directed graph into groups of independent nodes.
///
/// - `nodes` is a collection of nodes.
/// - `successors` returns a list of successors for a given node.
///
/// This function works like [`topological_sort`](self::topological_sort), but
/// rather than producing a single ordering of nodes, this function partitions
/// the nodes into groups: the first group contains all nodes with no
/// dependencies, the second group contains all nodes whose only dependencies
/// are in the first group, and so on.  Concatenating the groups produces a
/// valid topological sort regardless of how the nodes within each group are
/// reordered.  No guarantees are made about the order of nodes within each
/// group.
///
/// The function returns either `Ok` with a valid list of groups, or `Err` with
/// a (groups, remaining) tuple containing a (possibly empty) partial list of
/// groups, and a list of remaining nodes that could not be grouped due to
/// cycles.  In the error case, the strongly connected set(s) can then be found
/// using the
/// [`strongly_connected_components`](super::strongly_connected_components::strongly_connected_components)
/// function on the list of remaining nodes.
///
/// The current implementation uses a variation of [Kahn's
/// algorithm](https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm),
/// and runs in O(|V| + |E|) time.
#[allow(clippy::type_complexity)]
#[allow(dead_code)]
pub fn topological_sort_into_groups<N, FN, IN>(
    nodes: &[N],
    mut successors: FN,
) -> Result<Vec<Vec<N>>, (Vec<Vec<N>>, Vec<N>)>
where
    N: Eq + Hash + Clone,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = N>,
{
    if nodes.is_empty() {
        return Ok(Vec::new());
    }
    let mut succs_map = HashMap::<N, MutSet<N>, BuildHasher>::with_capacity_and_hasher(
        nodes.len(),
        default_hasher(),
    );
    let mut preds_map =
        HashMap::<N, usize, BuildHasher>::with_capacity_and_hasher(nodes.len(), default_hasher());
    for node in nodes.iter() {
        succs_map.insert(node.clone(), successors(node).into_iter().collect());
        preds_map.insert(node.clone(), 0);
    }

    for succs in succs_map.values() {
        for succ in succs.iter() {
            *preds_map
                .get_mut(succ)
                .unwrap_or_else(|| panic!("key missing from preds_map")) += 1;
        }
    }
    let mut groups = Vec::<Vec<N>>::new();
    let mut prev_group: Vec<N> = preds_map
        .iter()
        .filter_map(|(node, &num_preds)| {
            if num_preds == 0 {
                Some(node.clone())
            } else {
                None
            }
        })
        .collect();
    if prev_group.is_empty() {
        let remaining: Vec<N> = preds_map.into_iter().map(|(node, _)| node).collect();
        return Err((Vec::new(), remaining));
    }
    for node in &prev_group {
        preds_map.remove(node);
    }
    while !preds_map.is_empty() {
        let mut next_group = Vec::<N>::new();
        for node in &prev_group {
            for succ in &succs_map[node] {
                {
                    let num_preds = preds_map.get_mut(succ).unwrap();
                    *num_preds -= 1;
                    if *num_preds > 0 {
                        continue;
                    }
                }
                next_group.push(preds_map.remove_entry(succ).unwrap().0);
            }
        }
        groups.push(mem::replace(&mut prev_group, next_group));
        if prev_group.is_empty() {
            let remaining: Vec<N> = preds_map.into_iter().map(|(node, _)| node).collect();
            return Err((groups, remaining));
        }
    }
    groups.push(prev_group);
    Ok(groups)
}

// Separate nodes of a directed graph into [strongly connected
// components](https://en.wikipedia.org/wiki/Strongly_connected_component).
//
// A [path-based strong component
// algorithm](https://en.wikipedia.org/wiki/Path-based_strong_component_algorithm)
// is used.

struct Params<N, FN, IN>
where
    N: Clone + Hash + Eq,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = N>,
{
    preorders: HashMap<N, Option<usize>, BuildHasher>,
    c: usize,
    successors: FN,
    p: Vec<N>,
    s: Vec<N>,
    scc: Vec<Vec<N>>,
    scca: MutSet<N>,
}

impl<N, FN, IN> Params<N, FN, IN>
where
    N: Clone + Hash + Eq,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = N>,
{
    fn new(nodes: &[N], successors: FN) -> Self {
        Self {
            preorders: nodes.iter().map(|n| (n.clone(), None)).collect::<HashMap<
                N,
                Option<usize>,
                BuildHasher,
            >>(),
            c: 0,
            successors,
            p: Vec::new(),
            s: Vec::new(),
            scc: Vec::new(),
            scca: MutSet::default(),
        }
    }
}

fn recurse_onto<N, FN, IN>(v: &N, params: &mut Params<N, FN, IN>)
where
    N: Clone + Hash + Eq,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = N>,
{
    params.preorders.insert(v.clone(), Some(params.c));
    params.c += 1;
    params.s.push(v.clone());
    params.p.push(v.clone());
    for w in (params.successors)(v) {
        if !params.scca.contains(&w) {
            if let Some(pw) = params.preorders.get(&w).and_then(|w| *w) {
                while params.preorders[&params.p[params.p.len() - 1]].unwrap() > pw {
                    params.p.pop();
                }
            } else {
                recurse_onto(&w, params);
            }
        }
    }
    if params.p[params.p.len() - 1] == *v {
        params.p.pop();
        let mut component = Vec::new();
        while let Some(node) = params.s.pop() {
            component.push(node.clone());
            params.scca.insert(node.clone());
            params.preorders.remove(&node);
            if node == *v {
                break;
            }
        }
        params.scc.push(component);
    }
}

/// Partition nodes reachable from a starting point into strongly connected components.
///
/// - `start` is the node we want to explore the graph from.
/// - `successors` returns a list of successors for a given node.
///
/// The function returns a list of strongly connected components sets. It will contain
/// at least one component (the one containing the `start` node).
pub fn strongly_connected_components_from<N, FN, IN>(start: &N, successors: FN) -> Vec<Vec<N>>
where
    N: Clone + Hash + Eq,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = N>,
{
    let mut params = Params::new(&[], successors);
    recurse_onto(start, &mut params);
    params.scc
}

/// Compute the strongly connected component containing a given node.
///
/// - `node` is the node we want the strongly connected component for.
/// - `successors` returns a list of successors for a given node.
///
/// The function returns the strongly connected component containing the node,
/// which is guaranteed to contain at least `node`.
pub fn strongly_connected_component<N, FN, IN>(node: &N, successors: FN) -> Vec<N>
where
    N: Clone + Hash + Eq,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = N>,
{
    strongly_connected_components_from(node, successors)
        .pop()
        .unwrap()
}

/// Partition all strongly connected components in a graph.
///
/// - `nodes` is a collection of nodes.
/// - `successors` returns a list of successors for a given node.
///
/// The function returns a list of strongly connected components sets.
#[allow(dead_code)]
pub fn strongly_connected_components<N, FN, IN>(nodes: &[N], successors: FN) -> Vec<Vec<N>>
where
    N: Clone + Hash + Eq,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = N>,
{
    let mut params = Params::new(nodes, successors);
    while let Some(node) = params.preorders.keys().find(|_| true).cloned() {
        recurse_onto(&node, &mut params);
    }
    params.scc
}
