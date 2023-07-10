use crate::markup::{mark_id_ast_id_map::MarkIdAstIdMap, nodes::MarkupNode};
use std::fmt::Write;

pub type MarkNodeId = usize;

#[derive(Debug, Default)]
pub struct SlowPool {
    nodes: Vec<MarkupNode>,
}

impl SlowPool {
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

        // TODO delete children of old node, this requires SlowPool to be changed to
        // make sure the indexes still make sense after removal/compaction
    }

    pub fn debug_string(&self, mark_id_ast_id_map: &MarkIdAstIdMap) -> String {
        let mut ret_str = String::new();

        for (mark_node_id, node) in self.nodes.iter().enumerate() {
            let ast_node_id_str = match mark_id_ast_id_map.get(mark_node_id) {
                Ok(ast_id) => format!("{ast_id:?}"),
                Err(err) => format!("{err:?}"),
            };
            let ast_node_id: String = ast_node_id_str
                .chars()
                .filter(|c| c.is_ascii_digit())
                .collect();

            let mut child_str = String::new();

            let node_children = node.get_children_ids();

            if !node_children.is_empty() {
                child_str = format!("children: {node_children:?}");
            }

            write!(
                ret_str,
                "{}: {} ({}) ast_id {:?} {}",
                mark_node_id,
                node.node_type_as_string(),
                node.get_content(),
                ast_node_id.parse::<usize>().unwrap(),
                child_str
            )
            .unwrap();
        }

        ret_str
    }
}
