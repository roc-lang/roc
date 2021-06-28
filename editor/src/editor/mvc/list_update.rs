
use crate::editor::ed_error::EdResult;
use crate::editor::markup::attribute::Attributes;
use crate::editor::markup::nodes;
use crate::editor::markup::nodes::MarkupNode;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;
use crate::editor::syntax_highlight::HighlightStyle;
use crate::lang::ast::Expr2;
use crate::lang::pool::{PoolVec};

pub fn start_new_list(ed_model: &mut EdModel) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(&ed_model)?;

    let is_blank_node = curr_mark_node.is_blank();

    let expr2_node =
        Expr2::List{ 
            elem_var: ed_model.module.env.var_store.fresh(),
            elems: PoolVec::empty(ed_model.module.env.pool)
        };

    let mark_node_pool = &mut ed_model.markup_node_pool;

    ed_model.module.env.pool.set(ast_node_id, expr2_node);

    let left_bracket_node = MarkupNode::Text {
        content: nodes::LEFT_SQUARE_BR.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::new(),
        parent_id_opt: Some(curr_mark_node_id), // current node will be replaced with nested one
    };

    let left_bracket_node_id = mark_node_pool.add(left_bracket_node);

    let right_bracket_node = MarkupNode::Text {
        content: nodes::RIGHT_SQUARE_BR.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::new(),
        parent_id_opt: Some(curr_mark_node_id), // current node will be replaced with nested one
    };

    let right_bracket_node_id = mark_node_pool.add(right_bracket_node);

    let nested_node = MarkupNode::Nested {
        ast_node_id,
        children_ids: vec![left_bracket_node_id, right_bracket_node_id],
        parent_id_opt,
    };

    if is_blank_node {
        mark_node_pool.replace_node(curr_mark_node_id, nested_node);

        // remove data corresponding to Blank node
        ed_model.del_at_line(old_caret_pos.line, old_caret_pos.column)?;

        ed_model.simple_move_carets_right(nodes::LEFT_SQUARE_BR.len());

        // update GridNodeMap and CodeLines
        ed_model.insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            nodes::LEFT_SQUARE_BR,
            left_bracket_node_id,
        )?;

        ed_model.insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column + nodes::LEFT_SQUARE_BR.len(),
            nodes::RIGHT_SQUARE_BR,
            right_bracket_node_id,
        )?;

        Ok(InputOutcome::Accepted)
    } else {
        Ok(InputOutcome::Ignored)
    }
}