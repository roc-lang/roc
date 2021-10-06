use roc_ast::lang::core::ast::{ast_node_to_string, ASTNodeId};
use roc_ast::lang::core::expr::expr2::{Expr2, ExprId};
use roc_ast::mem_pool::pool_vec::PoolVec;
use roc_code_markup::markup::common_nodes::{
    new_blank_mn, new_comma_mn, new_left_square_mn, new_right_square_mn,
};
use roc_code_markup::markup::nodes::{self, MarkupNode};
use roc_code_markup::slow_pool::MarkNodeId;

use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::{MissingParent, UnexpectedASTNode};
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;
use crate::ui::text::text_pos::TextPos;

pub fn start_new_list(ed_model: &mut EdModel) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(ed_model)?;

    let is_blank_node = curr_mark_node.is_blank();
    let curr_mark_node_nls = curr_mark_node.get_newlines_at_end();

    let expr2_node = Expr2::List {
        elem_var: ed_model.module.env.var_store.fresh(),
        elems: PoolVec::empty(ed_model.module.env.pool),
    };

    ed_model
        .module
        .env
        .pool
        .set(ast_node_id.to_expr_id()?, expr2_node);

    let left_bracket_node_id = ed_model.add_mark_node(new_left_square_mn(
        ast_node_id.to_expr_id()?,
        Some(curr_mark_node_id),
    ));

    let right_bracket_node_id = ed_model.add_mark_node(new_right_square_mn(
        ast_node_id.to_expr_id()?,
        Some(curr_mark_node_id),
    ));

    let nested_node = MarkupNode::Nested {
        ast_node_id,
        children_ids: vec![left_bracket_node_id, right_bracket_node_id],
        parent_id_opt,
        newlines_at_end: curr_mark_node_nls,
    };

    if is_blank_node {
        ed_model
            .mark_node_pool
            .replace_node(curr_mark_node_id, nested_node);

        ed_model.del_blank_expr_node(old_caret_pos)?;

        ed_model.simple_move_carets_right(nodes::LEFT_SQUARE_BR.len());

        // update GridNodeMap and CodeLines
        EdModel::insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            nodes::LEFT_SQUARE_BR,
            left_bracket_node_id,
            &mut ed_model.grid_node_map,
            &mut ed_model.code_lines,
        )?;

        EdModel::insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column + nodes::LEFT_SQUARE_BR.len(),
            nodes::RIGHT_SQUARE_BR,
            right_bracket_node_id,
            &mut ed_model.grid_node_map,
            &mut ed_model.code_lines,
        )?;

        Ok(InputOutcome::Accepted)
    } else {
        Ok(InputOutcome::Ignored)
    }
}

// insert Blank at current position for easy code reuse
pub fn add_blank_child(
    new_child_index: usize,
    new_ast_child_index: usize,
    ed_model: &mut EdModel,
) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node: _,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(ed_model)?;

    let trip_result: EdResult<(ExprId, ExprId, MarkNodeId)> = if let Some(parent_id) = parent_id_opt
    {
        let parent = ed_model.mark_node_pool.get(parent_id);

        let list_ast_node_id = parent.get_ast_node_id();
        let list_ast_node = ed_model.module.env.pool.get(list_ast_node_id.to_expr_id()?);

        match list_ast_node {
            Expr2::List {
                elem_var: _,
                elems: _,
            } => {
                let blank_elt = Expr2::Blank;
                let blank_elt_id = ed_model.module.env.pool.add(blank_elt);

                Ok((blank_elt_id, list_ast_node_id.to_expr_id()?, parent_id))
            }
            _ => UnexpectedASTNode {
                required_node_type: "List".to_string(),
                encountered_node_type: ast_node_to_string(ast_node_id, ed_model.module.env.pool),
            }
            .fail(),
        }
    } else {
        MissingParent {
            node_id: curr_mark_node_id,
        }
        .fail()
    };

    let (blank_elt_id, list_ast_node_id, parent_id) = trip_result?;

    let list_ast_node = ed_model.module.env.pool.get(list_ast_node_id);

    match list_ast_node {
        Expr2::List { elem_var, elems } => {
            let mut new_elems: Vec<ExprId> =
                elems.iter(ed_model.module.env.pool).copied().collect();

            new_elems.insert(new_ast_child_index, blank_elt_id);

            let new_list_node = Expr2::List {
                elem_var: *elem_var,
                elems: PoolVec::new(new_elems.into_iter(), ed_model.module.env.pool),
            };

            ed_model
                .module
                .env
                .pool
                .set(list_ast_node_id, new_list_node);

            Ok(())
        }
        _ => UnexpectedASTNode {
            required_node_type: "List".to_string(),
            encountered_node_type: ast_node_to_string(ast_node_id, ed_model.module.env.pool),
        }
        .fail(),
    }?;

    let new_mark_children = update_mark_children(
        new_child_index,
        blank_elt_id,
        list_ast_node_id,
        old_caret_pos,
        parent_id_opt,
        ed_model,
    )?;

    let parent = ed_model.mark_node_pool.get_mut(parent_id);

    for (indx, child) in new_mark_children.iter().enumerate() {
        parent.add_child_at_index(new_child_index + indx, *child)?;
    }

    Ok(InputOutcome::Accepted)
}

// add a Blank child to the Nested mark node and update the caret
pub fn update_mark_children(
    new_child_index: usize,
    blank_elt_id: ExprId,
    list_ast_node_id: ExprId,
    old_caret_pos: TextPos,
    parent_id_opt: Option<MarkNodeId>,
    ed_model: &mut EdModel,
) -> EdResult<Vec<MarkNodeId>> {
    let blank_mark_node_id = ed_model.add_mark_node(new_blank_mn(
        ASTNodeId::AExprId(blank_elt_id),
        parent_id_opt,
    ));

    let mut children: Vec<MarkNodeId> = vec![];

    if new_child_index > 1 {
        let comma_mark_node_id =
            ed_model.add_mark_node(new_comma_mn(list_ast_node_id, parent_id_opt));

        ed_model.simple_move_carets_right(nodes::COMMA.len());

        EdModel::insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            nodes::COMMA,
            comma_mark_node_id,
            &mut ed_model.grid_node_map,
            &mut ed_model.code_lines,
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
    EdModel::insert_between_line(
        old_caret_pos.line,
        old_caret_pos.column + comma_shift,
        nodes::BLANK_PLACEHOLDER,
        blank_mark_node_id,
        &mut ed_model.grid_node_map,
        &mut ed_model.code_lines,
    )?;

    Ok(children)
}
