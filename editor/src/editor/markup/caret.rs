use crate::editor::{
    ed_error::{CaretNotFound, EdResult, NodeWithoutAttributes},
    markup::attribute::Caret,
    markup::nodes::MarkupNode,
    slow_pool::{SlowNodeId, SlowPool},
};
use snafu::ensure;

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
