use crate::{
    markup_error::MarkResult,
    slow_pool::{MarkNodeId, SlowPool},
    syntax_highlight::HighlightStyle,
};

use super::{
    attribute::Attributes, common_nodes::new_comma_mn, convert::from_def2::add_node,
    mark_id_ast_id_map::MarkIdAstIdMap,
};

use crate::markup_error::{
    ExpectedTextNodeSnafu, NestedNodeMissingChildSnafu, NestedNodeRequiredSnafu,
};
use roc_ast::{
    lang::{core::ast::ASTNodeId, env::Env},
    mem_pool::pool_str::PoolStr,
};
use roc_utils::{index_of, slice_get};
use std::fmt;
use std::fmt::Write;

#[derive(Debug)]
pub enum MarkupNode {
    Nested {
        children_ids: Vec<MarkNodeId>,
        parent_id_opt: Option<MarkNodeId>,
        newlines_at_end: usize,
    },
    Text {
        content: String,
        syn_high_style: HighlightStyle,
        attributes: Attributes,
        parent_id_opt: Option<MarkNodeId>,
        newlines_at_end: usize,
    },
    Blank {
        attributes: Attributes,
        parent_id_opt: Option<MarkNodeId>,
        newlines_at_end: usize,
    },
    Indent {
        indent_level: usize,
        parent_id_opt: Option<MarkNodeId>,
    },
}

impl MarkupNode {
    pub fn get_parent_id_opt(&self) -> Option<MarkNodeId> {
        match self {
            MarkupNode::Nested { parent_id_opt, .. } => *parent_id_opt,
            MarkupNode::Text { parent_id_opt, .. } => *parent_id_opt,
            MarkupNode::Blank { parent_id_opt, .. } => *parent_id_opt,
            MarkupNode::Indent { parent_id_opt, .. } => *parent_id_opt,
        }
    }

    pub fn get_children_ids(&self) -> Vec<MarkNodeId> {
        match self {
            MarkupNode::Nested { children_ids, .. } => children_ids.to_vec(),
            MarkupNode::Text { .. } => vec![],
            MarkupNode::Blank { .. } => vec![],
            MarkupNode::Indent { .. } => vec![],
        }
    }

    pub fn get_sibling_ids(&self, mark_node_pool: &SlowPool) -> Vec<MarkNodeId> {
        if let Some(parent_id) = self.get_parent_id_opt() {
            let parent_node = mark_node_pool.get(parent_id);

            parent_node.get_children_ids()
        } else {
            vec![]
        }
    }

    // return (index of child in list of children, closest ast index of child corresponding to ast node)
    pub fn get_child_indices(
        &self,
        mark_node_id: MarkNodeId,
        ast_node_id: ASTNodeId,
        mark_id_ast_id_map: &MarkIdAstIdMap,
    ) -> MarkResult<(usize, usize)> {
        match self {
            MarkupNode::Nested { children_ids, .. } => {
                let mut mark_child_index_opt: Option<usize> = None;
                let mut child_ids_with_ast: Vec<MarkNodeId> = Vec::new();

                for (indx, &mark_child_id) in children_ids.iter().enumerate() {
                    if mark_child_id == mark_node_id {
                        mark_child_index_opt = Some(indx);
                    }

                    let child_ast_node_id = mark_id_ast_id_map.get(mark_child_id)?;
                    // a node that points to the same ast_node as the parent is a ',', '[', ']'
                    // those are not "real" ast children
                    if child_ast_node_id != ast_node_id {
                        child_ids_with_ast.push(mark_child_id)
                    }
                }

                if let Some(child_index) = mark_child_index_opt {
                    if child_index == (children_ids.len() - 1) {
                        let ast_child_index = child_ids_with_ast.len();

                        Ok((child_index, ast_child_index))
                    } else {
                        // we want to find the index of the closest ast mark node to child_index
                        let mut indices_in_mark = vec![];

                        for &c_id in child_ids_with_ast.iter() {
                            indices_in_mark.push(index_of(c_id, children_ids)?);
                        }

                        let mut last_diff = usize::MAX;
                        let mut best_index = 0;

                        for index in indices_in_mark.iter() {
                            let curr_diff =
                                isize::abs((*index as isize) - (child_index as isize)) as usize;

                            if curr_diff >= last_diff {
                                break;
                            } else {
                                last_diff = curr_diff;
                                best_index = *index;
                            }
                        }

                        let closest_ast_child = slice_get(best_index, children_ids)?;

                        let closest_ast_child_index =
                            index_of(*closest_ast_child, &child_ids_with_ast)?;

                        // +1 because we want to insert after ast_child
                        Ok((child_index, closest_ast_child_index + 1))
                    }
                } else {
                    NestedNodeMissingChildSnafu {
                        node_id: mark_node_id,
                        children_ids: children_ids.clone(),
                    }
                    .fail()
                }
            }
            _ => NestedNodeRequiredSnafu {
                node_type: self.node_type_as_string(),
            }
            .fail(),
        }
    }

    pub fn get_content(&self) -> String {
        match self {
            MarkupNode::Nested { .. } => "".to_owned(),
            MarkupNode::Text { content, .. } => content.clone(),
            MarkupNode::Blank { .. } => BLANK_PLACEHOLDER.to_owned(),
            MarkupNode::Indent { indent_level, .. } => SINGLE_INDENT.repeat(*indent_level),
        }
    }

    // gets content and adds newline from newline_at_end
    pub fn get_full_content(&self) -> String {
        let mut full_content = self.get_content();

        for _ in 0..self.get_newlines_at_end() {
            full_content.push('\n')
        }

        full_content
    }

    pub fn get_content_mut(&mut self) -> MarkResult<&mut String> {
        match self {
            MarkupNode::Text { content, .. } => Ok(content),
            _ => ExpectedTextNodeSnafu {
                function_name: "set_content".to_owned(),
                node_type: self.node_type_as_string(),
            }
            .fail(),
        }
    }

    pub fn is_all_alphanumeric(&self) -> bool {
        self.get_content()
            .chars()
            .all(|chr| chr.is_ascii_alphanumeric())
    }

    pub fn add_child_at_index(&mut self, index: usize, child_id: MarkNodeId) -> MarkResult<()> {
        if let MarkupNode::Nested { children_ids, .. } = self {
            children_ids.splice(index..index, vec![child_id]);
        } else {
            NestedNodeRequiredSnafu {
                node_type: self.node_type_as_string(),
            }
            .fail()?;
        }

        Ok(())
    }

    pub fn node_type_as_string(&self) -> String {
        let type_str = match self {
            MarkupNode::Nested { .. } => "Nested",
            MarkupNode::Text { .. } => "Text",
            MarkupNode::Blank { .. } => "Blank",
            MarkupNode::Indent { .. } => "Indent",
        };

        type_str.to_owned()
    }

    pub fn is_blank(&self) -> bool {
        matches!(self, MarkupNode::Blank { .. })
    }

    pub fn is_nested(&self) -> bool {
        matches!(self, MarkupNode::Nested { .. })
    }

    pub fn get_newlines_at_end(&self) -> usize {
        match self {
            MarkupNode::Nested {
                newlines_at_end, ..
            } => *newlines_at_end,
            MarkupNode::Text {
                newlines_at_end, ..
            } => *newlines_at_end,
            MarkupNode::Blank {
                newlines_at_end, ..
            } => *newlines_at_end,
            MarkupNode::Indent { .. } => 0,
        }
    }

    pub fn add_newline_at_end(&mut self) {
        match self {
            MarkupNode::Nested {
                newlines_at_end, ..
            } => *newlines_at_end += 1,
            MarkupNode::Text {
                newlines_at_end, ..
            } => *newlines_at_end += 1,
            MarkupNode::Blank {
                newlines_at_end, ..
            } => *newlines_at_end += 1,
            _ => {}
        }
    }
}

pub fn make_nested_mn(children_ids: Vec<MarkNodeId>, newlines_at_end: usize) -> MarkupNode {
    MarkupNode::Nested {
        children_ids,
        parent_id_opt: None,
        newlines_at_end,
    }
}

pub fn get_string<'a>(env: &Env<'a>, pool_str: &PoolStr) -> String {
    pool_str.as_str(env.pool).to_owned()
}

pub const BLANK_PLACEHOLDER: &str = " ";
pub const LEFT_ACCOLADE: &str = "{ ";
pub const RIGHT_ACCOLADE: &str = " }";
pub const LEFT_SQUARE_BR: &str = "[ ";
pub const RIGHT_SQUARE_BR: &str = " ]";
pub const COLON: &str = ": ";
pub const COMMA: &str = ", ";
pub const DOT: &str = ".";
pub const STRING_QUOTES: &str = "\"\"";
pub const EQUALS: &str = " = ";
pub const ARROW: &str = " -> ";
pub const SINGLE_INDENT: &str = "    "; // 4 spaces

pub fn new_markup_node(
    text: String,
    node_id: ASTNodeId,
    highlight_style: HighlightStyle,
    mark_node_pool: &mut SlowPool,
    mark_id_ast_id_map: &mut MarkIdAstIdMap,
    indent_level: usize,
) -> MarkNodeId {
    let content_node = MarkupNode::Text {
        content: text,
        syn_high_style: highlight_style,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    };

    let content_node_id = add_node(content_node, node_id, mark_node_pool, mark_id_ast_id_map);

    if indent_level > 0 {
        let indent_node = MarkupNode::Indent {
            indent_level,
            parent_id_opt: None,
        };

        let indent_node_id = add_node(indent_node, node_id, mark_node_pool, mark_id_ast_id_map);

        let nested_node = MarkupNode::Nested {
            children_ids: vec![indent_node_id, content_node_id],
            parent_id_opt: None,
            newlines_at_end: 0,
        };

        add_node(nested_node, node_id, mark_node_pool, mark_id_ast_id_map)
    } else {
        content_node_id
    }
}

pub fn set_parent_for_all(markup_node_id: MarkNodeId, mark_node_pool: &mut SlowPool) {
    let node = mark_node_pool.get(markup_node_id);

    if let MarkupNode::Nested {
        children_ids,
        parent_id_opt: _,
        newlines_at_end: _,
    } = node
    {
        // need to clone because of borrowing issues
        let children_ids_clone = children_ids.clone();

        for child_id in children_ids_clone {
            set_parent_for_all_helper(child_id, markup_node_id, mark_node_pool);
        }
    }
}

pub fn set_parent_for_all_helper(
    markup_node_id: MarkNodeId,
    parent_node_id: MarkNodeId,
    mark_node_pool: &mut SlowPool,
) {
    let node = mark_node_pool.get_mut(markup_node_id);

    match node {
        MarkupNode::Nested {
            children_ids,
            parent_id_opt,
            ..
        } => {
            *parent_id_opt = Some(parent_node_id);

            // need to clone because of borrowing issues
            let children_ids_clone = children_ids.clone();

            for child_id in children_ids_clone {
                set_parent_for_all_helper(child_id, markup_node_id, mark_node_pool);
            }
        }
        MarkupNode::Text { parent_id_opt, .. } => *parent_id_opt = Some(parent_node_id),
        MarkupNode::Blank { parent_id_opt, .. } => *parent_id_opt = Some(parent_node_id),
        MarkupNode::Indent { parent_id_opt, .. } => *parent_id_opt = Some(parent_node_id),
    }
}

impl fmt::Display for MarkupNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} ({}, {})",
            self.node_type_as_string(),
            self.get_content(),
            self.get_newlines_at_end()
        )
    }
}

pub fn tree_as_string(root_node_id: MarkNodeId, mark_node_pool: &SlowPool) -> String {
    let mut full_string = "\n(mark_node_tree)\n".to_owned();

    let node = mark_node_pool.get(root_node_id);

    writeln!(full_string, "{} mn_id {}\n", node, root_node_id).unwrap();

    tree_as_string_helper(node, 1, &mut full_string, mark_node_pool);

    full_string
}

fn tree_as_string_helper(
    node: &MarkupNode,
    level: usize,
    tree_string: &mut String,
    mark_node_pool: &SlowPool,
) {
    for child_id in node.get_children_ids() {
        let child = mark_node_pool.get(child_id);
        let child_str = format!("{}", mark_node_pool.get(child_id)).replace('\n', "\\n");

        let mut full_str = std::iter::repeat("|--- ")
            .take(level)
            .collect::<Vec<&str>>()
            .join("")
            .to_owned();

        writeln!(full_str, "{} mn_id {}", child_str, child_id).unwrap();

        tree_string.push_str(&full_str);

        tree_as_string_helper(child, level + 1, tree_string, mark_node_pool);
    }
}

// return to the the root parent_id of the current node
pub fn get_root_mark_node_id(mark_node_id: MarkNodeId, mark_node_pool: &SlowPool) -> MarkNodeId {
    let mut curr_mark_node_id = mark_node_id;
    let mut curr_parent_id_opt = mark_node_pool.get(curr_mark_node_id).get_parent_id_opt();

    while let Some(curr_parent_id) = curr_parent_id_opt {
        curr_mark_node_id = curr_parent_id;
        curr_parent_id_opt = mark_node_pool.get(curr_mark_node_id).get_parent_id_opt();
    }

    curr_mark_node_id
}

// put space mark nodes between each node in mark_nodes
pub fn join_mark_nodes_spaces(
    mark_nodes_ids: Vec<MarkNodeId>,
    with_prepend: bool,
    mark_node_pool: &mut SlowPool,
) -> Vec<MarkNodeId> {
    let space_range_max = if with_prepend {
        mark_nodes_ids.len()
    } else {
        mark_nodes_ids.len() - 1
    };

    let join_nodes: Vec<MarkNodeId> = (0..space_range_max)
        .map(|_| {
            let space_node = MarkupNode::Text {
                content: " ".to_string(),
                syn_high_style: HighlightStyle::Blank,
                attributes: Attributes::default(),
                parent_id_opt: None,
                newlines_at_end: 0,
            };

            mark_node_pool.add(space_node)
        })
        .collect();

    if with_prepend {
        interleave(join_nodes.into_iter(), mark_nodes_ids)
    } else {
        interleave(mark_nodes_ids, join_nodes.into_iter())
    }
}

// put comma mark nodes between each node in mark_nodes
pub fn join_mark_nodes_commas(mark_nodes: Vec<MarkupNode>) -> Vec<MarkupNode> {
    let join_nodes: Vec<MarkupNode> = (0..(mark_nodes.len() - 1))
        .map(|_| new_comma_mn())
        .collect();

    interleave(mark_nodes.into_iter(), join_nodes)
}

pub fn mark_nodes_to_string(markup_node_ids: &[MarkNodeId], mark_node_pool: &SlowPool) -> String {
    let mut all_code_string = String::new();

    for mark_node_id in markup_node_ids.iter() {
        node_to_string_w_children(*mark_node_id, &mut all_code_string, mark_node_pool)
    }

    all_code_string
}

pub fn node_to_string_w_children(
    node_id: MarkNodeId,
    str_buffer: &mut String,
    mark_node_pool: &SlowPool,
) {
    let node = mark_node_pool.get(node_id);

    if node.is_nested() {
        for child_id in node.get_children_ids() {
            node_to_string_w_children(child_id, str_buffer, mark_node_pool);
        }
        for _ in 0..node.get_newlines_at_end() {
            str_buffer.push('\n')
        }
    } else {
        let node_content_str = node.get_full_content();

        str_buffer.push_str(&node_content_str);
    }
}

fn interleave<I, J>(i: I, j: J) -> Vec<I::Item>
where
    I: IntoIterator,
    J: IntoIterator<Item = I::Item>,
{
    let mut output = Vec::new();

    let mut flag = false;

    let mut i = i.into_iter();
    let mut j = j.into_iter();

    loop {
        flag = !flag;

        if flag {
            match i.next() {
                None => {
                    output.extend(j);
                    break output;
                }
                Some(v) => {
                    output.push(v);
                }
            }
        } else {
            match j.next() {
                None => {
                    output.extend(i);
                    break output;
                }
                Some(v) => {
                    output.push(v);
                }
            }
        }
    }
}
