use roc_types::subs::Variable;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Problem {
    RanOutOfNodeIds,
}

pub type Res<T> = Result<T, Problem>;

/// The index into a decl's array of nodes.
/// This is a u32 index because no decl is allowed to hold more than 2^32 entries,
/// and it saves space on 64-bit systems compared to a pointer.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeId(u32);

impl NodeId {
    pub const NONE: NodeId = NodeId(std::u32::MAX);

    pub fn as_index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self == &NodeId::NONE {
            write!(f, "none")
        } else {
            write!(f, "#{}", self.0)
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Nodes {
    nodes: Vec<Node>,
}

impl Nodes {
    pub fn push_expr(&mut self, parent_id: NodeId, expr: Expr) -> Res<NodeId> {
        let node_id = self.push_node(Node {
            parent_id,
            content: NodeContent::Expr(expr),
        })?;

        self.set_child(parent_id, node_id);

        Ok(node_id)
    }

    fn set_child(&mut self, parent_id: NodeId, _child_id: NodeId) {
        match self.nodes.get_mut(parent_id.as_index()) {
            Some(parent) => match &mut parent.content {
                NodeContent::Expr(Expr::Int { .. }) | NodeContent::Expr(Expr::Float { .. }) => {
                    // This node has no children. No further action needed!
                }
                NodeContent::Caret { .. } => {
                    // This node has no children. No further action needed!
                }
                other => {
                    todo!("handle set_child for {:?}", other);
                }
            },
            None => {
                // The only reason this bounds check should ever fail
                // is that we were passed no parent.
                debug_assert!(parent_id == NodeId::NONE);
            }
        }
    }

    // fn remove_node(&mut self, target_id: NodeId) {
    //     debug_assert!(
    //         target_id != NodeId::NONE,
    //         "Called remove_node on NodeId::NONE"
    //     );

    //     {
    //         let target = self
    //             .nodes
    //             .get_mut(target_id.as_index())
    //             .unwrap_or_else(|| panic!("Tried to remove nonexistant node {:?}", target_id));
    //         let opt_parent = self.nodes.get_mut(target.parent_id.as_index());

    //         match &mut target.content {
    //             NodeContent::Expr(Expr::Int { .. }) => {
    //                 // This node has no children, so remove it from its parent and move on.
    //                 match opt_parent {
    //                     Some(parent) => parent.remove_child(target_id),
    //                     None => {
    //                         // The only valid reason the node's parent_id might
    //                         // not have been in the nodes vec is that it's NONE.
    //                         debug_assert!(target.parent_id == NodeId::NONE);
    //                     }
    //                 }
    //             }

    //             NodeContent::Caret { child_id, offset } => {
    //                 match opt_parent {
    //                     Some(parent) => {
    //                         // Go into the parent and replace this caret with
    //                         // the new child_id
    //                         // parent.replace_child(target_id, child_id)
    //                     }
    //                     None => {
    //                         // The only valid reason the node's parent_id might
    //                         // not have been in the nodes vec is that it's NONE.
    //                         debug_assert!(target.parent_id == NodeId::NONE);
    //                     }
    //                 }
    //             }

    //             NodeContent::Removed => {
    //                 panic!("Tried to remove a node that was already removed.");
    //             }
    //         }
    //     }
    // }

    fn push_node(&mut self, node: Node) -> Res<NodeId> {
        // TODO check to see if we have any removed nodes, and if so, use those
        // instead of pushing to the end. This way, we can avoid needing to defrag.

        // The length *before* we pushed == the index of the node we pushed.
        let index = self.nodes.len();

        self.nodes.push(node);

        if index < std::u32::MAX as usize {
            Ok(NodeId(index as u32))
        } else {
            // u32::MAX is reserved for NodeId::NONE, so if we hit that on a
            // saturating add, we've overflowed. Game over.
            Err(Problem::RanOutOfNodeIds)
        }
    }

    pub fn wrap_with_caret(&mut self, target_id: NodeId, offset: u16) -> Res<NodeId> {
        let parent_id = self.get(target_id).parent_id;
        let caret_id = {
            let content = NodeContent::Caret {
                child_id: target_id,
                offset,
            };

            self.push_node(Node { content, parent_id })?
        };

        // Now that we have the caret_id, update the old
        if parent_id != NodeId::NONE {
            self.get_mut(target_id).replace_child(target_id, caret_id);
        }

        Ok(caret_id)
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn split_at_mut(&mut self, index: NodeId) -> (&mut [Node], &mut [Node]) {
        self.nodes.split_at_mut(index.0 as usize)
    }

    pub fn get(&self, index: NodeId) -> &Node {
        self.nodes
            .get(index.0 as usize)
            .unwrap_or_else(|| panic!("Unable to find node at index {:?}", index.0))
    }

    pub fn get_mut(&mut self, index: NodeId) -> &mut Node {
        self.nodes
            .get_mut(index.0 as usize)
            .unwrap_or_else(|| panic!("Unable to find node at index {:?}", index.0))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Node {
    parent_id: NodeId,
    content: NodeContent,
}

#[derive(Debug, PartialEq, Eq)]
pub enum NodeContent {
    Expr(Expr /* TODO should Node have a Variable? */),
    // Pattern(Pattern),
    Caret { child_id: NodeId, offset: u16 },
    // SelectionStart { offset: u16, child: NodeId },
    // SelectionEnd { offset: u16, child: NodeId },
    Removed,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    /// An integer literal (without a dot)
    Int { text: String, var: Variable },
    /// An floating-point literal (with a dot)
    Float { text: String, var: Variable },
    // /// A partial lookup that has not yet been completed, e.g.
    // /// `Foo.` or `pkg.Foo.Bar`
    // PartialLookup {
    //     /// dot-separated sections, e.g. `Foo.Bar.` would be ["Foo", "Bar", ""]
    //     sections: Vec<String>,
    //     var: Variable,
    // },
    // Lookup {
    //     name: String,
    //     var: Variable,
    // },
    // If {
    //     conditional: NodeId,
    //     then_node: NodeId,
    //     else_node: NodeId,
    //     var: Variable,
    // },
    // Then {
    //     child: NodeId,
    //     var: Variable,
    // },
    // Else {
    //     child: NodeId,
    //     var: Variable,
    // },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Pattern {
    Identifier { text: String, var: Variable },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Decl {
    Def(Def),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Def {
    Body { pattern: NodeId, expr: NodeId },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub nodes: Nodes,
    pub decls: Vec<Decl>,
    // Use a Vec over a Set because it'll always be of small length
    pub carets: Vec<NodeId>,
    pub selections: Vec<(NodeId, NodeId)>,
}

impl Node {
    /// Returns the number of characters removed, so that
    /// the caret can be moved back this many indices.
    /// (For example, if you backspace over an emoji, that can move the
    /// caret back multiple indices.)
    pub fn backspace(&mut self, index: u16) -> u16 {
        use Expr::*;

        match &mut self.content {
            NodeContent::Expr(Int { text, .. }) | NodeContent::Expr(Float { text, .. }) => {
                // TODO remove an entire *grapheme cluster* here, not just a
                // single character! This will require using a 3rd-party crate.
                let removed = text.remove(index as usize);

                // TODO need to re-index any other carets/selections which
                // were also in this node - they need to be moved too!
                removed.len_utf8() as u16
            }
            other => {
                todo!("handle backspace for {:?}", other);
            }
        }
    }

    // fn remove_child(&mut self, id_to_remove: NodeId) {
    //     match &mut self.content {
    //         NodeContent::Expr(Expr::Int { .. }) | NodeContent::Expr(Expr::Float { .. }) => {
    //             // This node has no children, so no action is needed.
    //         }

    //         NodeContent::Caret { child_id, .. } => {
    //             if *child_id == id_to_remove {
    //                 *child_id = NodeId::NONE;
    //             }
    //         }

    //         NodeContent::Removed => {
    //             panic!(
    //                 "Tried to remove a child node ({:?}) from a NodeContent::Removed",
    //                 id_to_remove
    //             );
    //         }
    //     }
    // }

    fn replace_child(&mut self, old_id: NodeId, new_id: NodeId) {
        match &mut self.content {
            NodeContent::Expr(Expr::Int { .. }) | NodeContent::Expr(Expr::Float { .. }) => {
                // This node has no children, so no action is needed.
            }

            NodeContent::Caret { child_id, .. } => {
                if *child_id == old_id {
                    *child_id = new_id;
                }
            }

            NodeContent::Removed => {
                panic!(
                    "Tried to replace child node ID {:?} with {:?} in a NodeContent::Removed",
                    old_id, new_id
                );
            }
        }
    }
}

impl Module {
    pub fn backspace(&mut self) {
        if self.nodes.is_empty() {
            return;
        }

        for &caret_node_id in self.carets.iter() {
            debug_assert!(caret_node_id.as_index() <= self.nodes.len());

            // Use slices around the caret because we'll need to modify the
            // child node. Without these slices we'd need multiple simultaneous
            // mutable references to self.nodes, which is never allowed.
            let (before_caret, caret_and_after) = self.nodes.split_at_mut(caret_node_id);
            dbg!(&caret_node_id, &before_caret, &caret_and_after);
            let (caret_only, after_caret) = caret_and_after.split_at_mut(1);

            let caret = caret_only.first_mut().unwrap();
            let parent_id = caret.parent_id;

            match &mut caret.content {
                NodeContent::Caret { offset, child_id } => {
                    let child_id = *child_id;
                    let offset_index = *offset;

                    if offset_index != 0 {
                        // Get the child node from the appropriate slice
                        let child_node: &mut Node = {
                            debug_assert!(
                                child_id != caret_node_id,
                                "A caret had itself as its own child: {:?}",
                                caret_node_id
                            );

                            if child_id > caret_node_id {
                                after_caret.get_mut(child_id.as_index() - (before_caret.len() + 1))
                            } else {
                                before_caret.get_mut(child_id.as_index())
                            }
                        }
                        .unwrap_or_else(|| {
                            panic!("Could not get child node for caret {:?}", caret_node_id)
                        });

                        // Mutate the child node to apply the backspace operation.
                        //
                        // -1 because index 0 is *before* the first character
                        // in the string (which we already ruled out using the
                        // above conditional), and child_node.backspace expects
                        // to be given the index *after* the char to be removed.
                        let chars_removed = child_node.backspace(offset_index - 1);

                        // Mutate the caret to decrement its offset
                        *offset = offset_index - chars_removed;
                    } else {
                        todo!(
                            "Backspace when caret offset is 0, into parent: {:?}",
                            parent_id
                        );
                    }
                }
                other => {
                    unreachable!("Caret pointed to a non-caret node: {:?}", other);
                }
            }
        }
    }
}

#[test]
fn single_backspace_1_caret() {
    use roc_types::subs::VarStore;

    let var_store = VarStore::default();
    let int_var = var_store.fresh();
    let int_node_id;
    let caret_node_id;
    let expected = {
        let mut nodes = Nodes::default();

        int_node_id = nodes
            .push_expr(
                NodeId::NONE,
                Expr::Int {
                    text: "abd".into(),
                    var: int_var,
                },
            )
            .unwrap();

        caret_node_id = nodes.wrap_with_caret(int_node_id, 2).unwrap();

        nodes
    };

    let actual = {
        let mut nodes = Nodes::default();

        let actual_node_id = nodes
            .push_expr(
                NodeId::NONE,
                Expr::Int {
                    text: "abcd".into(),
                    var: int_var,
                },
            )
            .unwrap();

        assert!(int_node_id == actual_node_id);

        let actual_node_id = nodes.wrap_with_caret(int_node_id, 3).unwrap();

        assert!(caret_node_id == actual_node_id);

        nodes
    };

    let mut module = Module {
        nodes: actual,
        decls: Vec::new(),
        carets: vec![caret_node_id],
        selections: Vec::new(),
    };

    module.backspace();

    assert_eq!(expected, module.nodes);
}

#[test]
fn double_backspace_1_caret() {
    use roc_types::subs::VarStore;

    let var_store = VarStore::default();
    let int_var = var_store.fresh();
    let int_node_id;
    let caret_node_id;
    let expected = {
        let mut nodes = Nodes::default();

        int_node_id = nodes
            .push_expr(
                NodeId::NONE,
                Expr::Int {
                    text: "ad".into(),
                    var: int_var,
                },
            )
            .unwrap();

        caret_node_id = nodes.wrap_with_caret(int_node_id, 1).unwrap();

        nodes
    };

    let actual = {
        let mut nodes = Nodes::default();

        let actual_node_id = nodes
            .push_expr(
                NodeId::NONE,
                Expr::Int {
                    text: "abcd".into(),
                    var: int_var,
                },
            )
            .unwrap();

        assert!(int_node_id == actual_node_id);

        let actual_node_id = nodes.wrap_with_caret(int_node_id, 3).unwrap();

        assert!(caret_node_id == actual_node_id);

        nodes
    };

    let mut module = Module {
        nodes: actual,
        decls: Vec::new(),
        carets: vec![caret_node_id],
        selections: Vec::new(),
    };

    module.backspace();
    module.backspace();

    assert_eq!(expected, module.nodes);
}
