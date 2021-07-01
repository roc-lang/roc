use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::{MissingParent, UnexpectedASTNode};
use crate::editor::markup::attribute::Attributes;
use crate::editor::markup::nodes;
use crate::editor::markup::nodes::MarkupNode;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::syntax_highlight::HighlightStyle;
use crate::lang::ast::Expr2;
use crate::lang::ast::{expr2_to_string, ExprId};
use crate::lang::pool::PoolVec;
use crate::ui::text::text_pos::TextPos;

pub fn start_new_list(ed_model: &mut EdModel) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(&ed_model)?;

    let is_blank_node = curr_mark_node.is_blank();

    let expr2_node = Expr2::List {
        elem_var: ed_model.module.env.var_store.fresh(),
        elems: PoolVec::empty(ed_model.module.env.pool),
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

// insert Blank at current position for easy code reuse
pub fn add_blank_child(ed_model: &mut EdModel) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node: _,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(&ed_model)?;

    let trip_result: EdResult<(ExprId, usize, ExprId, MarkNodeId)> =
        if let Some(parent_id) = parent_id_opt {
            let parent = ed_model.markup_node_pool.get(parent_id);

            let new_child_index = parent.get_children_ids().len() - 1; // TODO support adding child at place other than end

            let list_ast_node_id = parent.get_ast_node_id();
            let list_ast_node = ed_model.module.env.pool.get(list_ast_node_id);

            match list_ast_node {
                Expr2::List {
                    elem_var: _,
                    elems: _,
                } => {
                    let blank_elt = Expr2::Blank;
                    let blank_elt_id = ed_model.module.env.pool.add(blank_elt);

                    Ok((blank_elt_id, new_child_index, list_ast_node_id, parent_id))
                }
                _ => UnexpectedASTNode {
                    required_node_type: "List".to_string(),
                    encountered_node_type: expr2_to_string(ast_node_id, ed_model.module.env.pool),
                }
                .fail(),
            }
        } else {
            MissingParent {
                node_id: curr_mark_node_id,
            }
            .fail()
        };

    let (blank_elt_id, new_child_index, list_ast_node_id, parent_id) = trip_result?;

    let new_mark_children = make_mark_children(
        new_child_index,
        blank_elt_id,
        list_ast_node_id,
        old_caret_pos,
        parent_id_opt,
        ed_model,
    )?;

    let parent = ed_model.markup_node_pool.get_mut(parent_id);

    for (indx, child) in new_mark_children.iter().enumerate() {
        parent.add_child_at_index(new_child_index + indx, *child)?;
    }

    //TODO add ast children

    Ok(InputOutcome::Accepted)
}

pub fn make_mark_children(
    new_child_index: usize,
    blank_elt_id: ExprId,
    list_ast_node_id: ExprId,
    old_caret_pos: TextPos,
    parent_id_opt: Option<MarkNodeId>,
    ed_model: &mut EdModel,
) -> EdResult<Vec<MarkNodeId>> {
    let blank_mark_node = MarkupNode::Blank {
        ast_node_id: blank_elt_id,
        syn_high_style: HighlightStyle::Blank,
        attributes: Attributes::new(),
        parent_id_opt,
    };

    let blank_mark_node_id = ed_model.markup_node_pool.add(blank_mark_node);

    let mut children: Vec<MarkNodeId> = vec![];

    if new_child_index > 1 {
        let comma_mark_node = MarkupNode::Text {
            content: nodes::COMMA.to_owned(),
            ast_node_id: list_ast_node_id,
            syn_high_style: HighlightStyle::Blank,
            attributes: Attributes::new(),
            parent_id_opt,
        };

        let comma_mark_node_id = ed_model.markup_node_pool.add(comma_mark_node);

        ed_model.simple_move_carets_right(nodes::COMMA.len());

        ed_model.insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            nodes::COMMA,
            comma_mark_node_id,
        )?;

        children.push(comma_mark_node_id);
    }

    children.push(blank_mark_node_id);

    let comma_shift = if new_child_index == 1 {
        0
    } else {
        nodes::COMMA.len()
    };

    // update GridNodeMap and CodeLines
    ed_model.insert_between_line(
        old_caret_pos.line,
        old_caret_pos.column + comma_shift,
        nodes::BLANK_PLACEHOLDER,
        blank_mark_node_id,
    )?;

    Ok(children)
}
