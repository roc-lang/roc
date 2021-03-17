
use crate::editor::{
    markup::nodes::{MarkupNode},
    markup::attribute::{Caret},
    slow_pool::{SlowNodeId, SlowPool},
    ed_error::{EdResult, CaretNotFound},
};
use snafu::{ensure};

// Returns id of node that has Caret attribute
pub fn set_caret_at_start(
    markup_node_id: SlowNodeId,
    markup_node_pool: &mut SlowPool,
) -> SlowNodeId {
    let markup_node = markup_node_pool.get_mut(markup_node_id);

    match markup_node {
        MarkupNode::Nested {
            ast_node_id: _,
            children_ids,
            parent_id_opt: _,
        } => {
            if let Some(child_id) = children_ids.first() {
                set_caret_at_start(*child_id, markup_node_pool)
            } else {
                //TODO use result instead
                unreachable!()
            }
        }
        MarkupNode::Text {
            content: _,
            ast_node_id: _,
            syn_high_style: _,
            attributes,
            parent_id_opt: _,
        } => {
            attributes.add(Caret::new_attr(0));
            markup_node_id
        }
        MarkupNode::Blank {
            ast_node_id: _,
            attributes,
            syn_high_style: _,
            parent_id_opt: _,
        } => {
            attributes.add(Caret::new_attr(0));
            markup_node_id
        }
    }
}

// returns node containing the caret after the move
pub fn move_carets_right_for_node(
    node_with_caret_id: SlowNodeId,
    markup_node_pool: &mut SlowPool,
) -> EdResult<Vec<SlowNodeId>> {
    let carets = get_carets(node_with_caret_id, markup_node_pool)?;
    let node_content = markup_node_pool.get(node_with_caret_id).get_content()?;
    
    ensure!(!carets.is_empty(), CaretNotFound{node_content: "TODO".to_owned()});

    let mut new_nodes_w_carets = Vec::new();

    for caret in carets {
        
        let new_node = 
            if caret.offset_col + 1 < node_content.len() {
                increase_caret_offset(node_with_caret_id, caret.offset_col, markup_node_pool)?;

                node_with_caret_id
            } else {
                // TODO
                delete_caret(node_with_caret_id);

                let next_child_opt = markup_node_pool.get(node_with_caret_id).get_next_leaf(node_with_caret_id, markup_node_pool)?;

                if let Some(next_child_id) = next_child_opt {
                    let next_child = markup_node_pool.get_mut(next_child_id);
                    let child_attrs = next_child.get_mut_attributes()?;
                    child_attrs.add_caret(0);

                    next_child_id
                } else {
                    node_with_caret_id
                }
            };

        new_nodes_w_carets.push(new_node);
    }

    Ok(new_nodes_w_carets)
}

fn get_carets(node_with_caret_id: SlowNodeId, markup_node_pool: &SlowPool) -> EdResult<Vec<Caret>> {
    let curr_node = markup_node_pool.get(node_with_caret_id);
    let attributes = curr_node.get_attributes()?;
    
    Ok(attributes.get_carets())
}

// this method assumes the current caret position is checked and increasing it will not lead to an invalid caret position
fn increase_caret_offset(node_id: SlowNodeId, offset_col: usize, markup_node_pool: &mut SlowPool) -> EdResult<()> {
    let node = markup_node_pool.get_mut(node_id);
    let attrs = node.get_mut_attributes()?;
    let mut carets = attrs.get_mut_carets();

    for caret in carets.iter_mut(){
        if caret.offset_col == offset_col {
            caret.offset_col += 1;
        }
    }

    Ok(())
}