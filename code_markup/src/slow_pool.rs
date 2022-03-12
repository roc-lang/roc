use std::fmt;

use crate::markup::nodes::MarkupNode;

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
}

impl fmt::Display for SlowPool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\n\n(mark_node_pool)\n")?;

        for (index, node) in self.nodes.iter().enumerate() {
            let ast_node_id_str = format!("{:?}", node.get_ast_node_id());
            let ast_node_id: String = ast_node_id_str
                .chars()
                .filter(|c| c.is_ascii_digit())
                .collect();

            let mut child_str = String::new();

            let node_children = node.get_children_ids();

            if !node_children.is_empty() {
                child_str = format!("children: {:?}", node_children);
            }

            writeln!(
                f,
                "{}: {} ({}) ast_id {:?} {}",
                index,
                node.node_type_as_string(),
                node.get_content(),
                ast_node_id.parse::<usize>().unwrap(),
                child_str
            )?;
        }

        Ok(())
    }
}
