use crate::util::flat_slices::FlatSlices;
use crate::util::id_type::{Count, Id};
use crate::util::id_vec::IdVec;
use crate::util::replace_none::replace_none;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SccKind {
    Acyclic,
    Cyclic,
}

pub fn strongly_connected<SccId, NodeId, NodeSuccessors>(
    node_count: Count<NodeId>,
    mut node_successors: impl FnMut(NodeId) -> NodeSuccessors,
) -> FlatSlices<SccId, SccKind, NodeId>
where
    SccId: Id,
    NodeId: Id + Eq + Copy,
    NodeSuccessors: Iterator<Item = NodeId>,
{
    // We use Tarjan's algorithm, performing the depth-first search using an explicit Vec-based
    // stack instead of recursion to avoid stack overflows on large graphs.

    #[derive(Clone, Copy, Debug)]
    enum NodeState {
        Unvisited,
        OnSearchStack { index: u32, low_link: u32 },
        OnSccStack { index: u32 },
        Complete,
    }

    #[derive(Clone, Copy)]
    enum Action<NodeId> {
        TryVisit {
            parent: Option<NodeId>,
            node: NodeId,
        },
        FinishVisit {
            parent: Option<NodeId>,
            node: NodeId,
        },
    }

    let mut sccs = FlatSlices::new();

    let mut node_states = IdVec::filled_with(node_count, || NodeState::Unvisited);
    let mut node_self_loops = IdVec::filled_with(node_count, || None);
    let mut scc_stack = Vec::new();
    let mut search_stack = Vec::new();
    let mut next_index = 0;

    for search_root in node_count.iter() {
        search_stack.push(Action::TryVisit {
            parent: None,
            node: search_root,
        });
        while let Some(action) = search_stack.pop() {
            match action {
                Action::TryVisit { parent, node } => match node_states[node] {
                    NodeState::Unvisited => {
                        node_states[node] = NodeState::OnSearchStack {
                            index: next_index,
                            low_link: next_index,
                        };
                        next_index += 1;
                        scc_stack.push(node);

                        search_stack.push(Action::FinishVisit { parent, node });
                        // We need to explicitly track self-loops so that when we obtain a size-1
                        // SCC we can determine if it's cyclic or acyclic.
                        let mut has_self_loop = false;
                        for successor in node_successors(node) {
                            if successor == node {
                                has_self_loop = true;
                            }
                            search_stack.push(Action::TryVisit {
                                parent: Some(node),
                                node: successor,
                            });
                        }
                        replace_none(&mut node_self_loops[node], has_self_loop).unwrap();
                    }

                    NodeState::OnSearchStack { index, low_link: _ }
                    | NodeState::OnSccStack { index } => {
                        if let Some(parent) = parent {
                            if let NodeState::OnSearchStack {
                                index: _,
                                low_link: parent_low_link,
                            } = &mut node_states[parent]
                            {
                                *parent_low_link = (*parent_low_link).min(index);
                            } else {
                                unreachable!("parent should be on search stack");
                            }
                        }
                    }

                    NodeState::Complete => {}
                },

                Action::FinishVisit { parent, node } => {
                    let (index, low_link) =
                        if let NodeState::OnSearchStack { index, low_link } = node_states[node] {
                            (index, low_link)
                        } else {
                            unreachable!("node should be on search stack");
                        };

                    node_states[node] = NodeState::OnSccStack { index };

                    if let Some(parent) = parent {
                        if let NodeState::OnSearchStack {
                            index: _,
                            low_link: parent_low_link,
                        } = &mut node_states[parent]
                        {
                            *parent_low_link = (*parent_low_link).min(low_link);
                        } else {
                            unreachable!("parent should be on search stack")
                        }
                    }

                    if low_link == index {
                        let mut scc_start = scc_stack.len();
                        loop {
                            scc_start -= 1;
                            let scc_node = scc_stack[scc_start];
                            debug_assert!(matches!(
                                node_states[scc_node],
                                NodeState::OnSccStack { .. }
                            ));
                            node_states[scc_node] = NodeState::Complete;
                            if scc_node == node {
                                break;
                            }
                        }
                        let scc_slice = &scc_stack[scc_start..];
                        let scc_kind = if scc_slice.len() == 1 && !node_self_loops[node].unwrap() {
                            SccKind::Acyclic
                        } else {
                            SccKind::Cyclic
                        };
                        sccs.push_slice(scc_kind, scc_slice);
                        scc_stack.truncate(scc_start);
                    }
                }
            }
        }
    }

    sccs
}
