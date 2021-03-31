use crate::editor::markup::nodes::MarkupNode;

pub type MarkNodeId = usize;

#[derive(Debug)]
pub struct SlowPool {
    nodes: Vec<MarkupNode>,
}

impl SlowPool {
    pub fn new() -> SlowPool {
        SlowPool { nodes: Vec::new() }
    }

    pub fn add(&mut self, node: MarkupNode) -> MarkNodeId {
        let id = self.nodes.len();

        self.nodes.push(node);

        id
    }

    pub fn get(&self, node_id: MarkNodeId) -> &MarkupNode {
        // unwrap because Pool doesn't return Result either
        self.nodes.get(node_id).unwrap()
    }

    pub fn get_mut(&mut self, node_id: MarkNodeId) -> &mut MarkupNode {
        // unwrap because Pool doesn't return Result either
        self.nodes.get_mut(node_id).unwrap()
    }

    pub fn replace_node(&mut self, node_id: MarkNodeId, new_node: MarkupNode) {
        self.nodes[node_id] = new_node;
    }
}
