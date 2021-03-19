use crate::editor::mvc::ed_model::LeafIndex;
use crate::editor::{
    ed_error::{CaretNotFound, EdResult, NodeWithoutAttributes},
    markup::attribute::Caret,
    markup::nodes::MarkupNode,
    slow_pool::{SlowNodeId, SlowPool},
};
use snafu::ensure;

// Returns id of node that has Caret attribute
pub fn set_caret_at_start(
    markup_node_id: SlowNodeId,
    markup_node_pool: &mut SlowPool,
) -> EdResult<SlowNodeId> {
    let markup_node = markup_node_pool.get_mut(markup_node_id);

    match markup_node {
        MarkupNode::Nested {
            ast_node_id: _,
            children_ids: _,
            parent_id_opt: _,
        } => NodeWithoutAttributes {}.fail(),
        MarkupNode::Text {
            content: _,
            ast_node_id: _,
            syn_high_style: _,
            attributes,
            parent_id_opt: _,
        } => {
            attributes.add(Caret::new_attr(0));
            Ok(markup_node_id)
        }
        MarkupNode::Blank {
            ast_node_id: _,
            attributes,
            syn_high_style: _,
            parent_id_opt: _,
        } => {
            attributes.add(Caret::new_attr(0));
            Ok(markup_node_id)
        }
    }
}

// Returns nodes containing the carets after the move, as well as its position in a DFS ordered list of leaves.
pub fn move_carets_right_for_node(
    node_with_caret_id: SlowNodeId,
    caret_id_leaf_index: LeafIndex,
    next_leaf_id_opt: Option<SlowNodeId>,
    markup_node_pool: &mut SlowPool,
) -> EdResult<Vec<(SlowNodeId, LeafIndex)>> {
    let carets = get_carets(node_with_caret_id, markup_node_pool)?;
    let node_content = markup_node_pool.get(node_with_caret_id).get_content()?;

    ensure!(
        !carets.is_empty(),
        CaretNotFound {
            node_id: node_with_caret_id
        }
    );

    let mut new_nodes_w_carets = Vec::new();

    for caret in carets {
        let (new_node, new_leaf_index) = if caret.offset_col + 1 < node_content.len() {
            increase_caret_offset(node_with_caret_id, caret.offset_col, markup_node_pool)?;

            (node_with_caret_id, caret_id_leaf_index)
        } else if let Some(next_child_id) = next_leaf_id_opt {
            delete_caret(node_with_caret_id, caret.offset_col, markup_node_pool)?;

            let next_child = markup_node_pool.get_mut(next_child_id);
            let child_attrs = next_child.get_mut_attributes()?;
            child_attrs.add_caret(0);

            (next_child_id, caret_id_leaf_index + 1)
        } else if caret.offset_col + 1 == node_content.len() {
            // For last char in editor.
            // In other places we jump to start of next node instead of going to end of
            // this node, otherwise it would be like there is a space between every node.
            increase_caret_offset(node_with_caret_id, caret.offset_col, markup_node_pool)?;
            (node_with_caret_id, caret_id_leaf_index)
        } else {
            // Caret is at its end, keep it here.
            (node_with_caret_id, caret_id_leaf_index)
        };

        new_nodes_w_carets.push((new_node, new_leaf_index));
    }

    Ok(new_nodes_w_carets)
}

fn get_carets(node_with_caret_id: SlowNodeId, markup_node_pool: &SlowPool) -> EdResult<Vec<Caret>> {
    let curr_node = markup_node_pool.get(node_with_caret_id);
    let attributes = curr_node.get_attributes()?;

    Ok(attributes.get_carets())
}

// this method assumes the current caret position is checked and increasing it will not lead to an invalid caret position
fn increase_caret_offset(
    node_id: SlowNodeId,
    offset_col: usize,
    markup_node_pool: &mut SlowPool,
) -> EdResult<()> {
    let node = markup_node_pool.get_mut(node_id);
    let attrs = node.get_mut_attributes()?;
    let mut carets = attrs.get_mut_carets();

    for caret in carets.iter_mut() {
        if caret.offset_col == offset_col {
            caret.offset_col += 1;
        }
    }

    Ok(())
}

fn delete_caret(
    node_id: SlowNodeId,
    offset_col: usize,
    markup_node_pool: &mut SlowPool,
) -> EdResult<()> {
    let node = markup_node_pool.get_mut(node_id);
    let attrs = node.get_mut_attributes()?;
    attrs.delete_caret(offset_col, node_id)?;

    Ok(())
}
