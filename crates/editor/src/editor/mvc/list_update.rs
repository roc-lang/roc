use roc_ast::lang::core::ast::ast_node_to_string;
use roc_ast::lang::core::expr::expr2::{Expr2, ExprId};
use roc_ast::mem_pool::pool_vec::PoolVec;
use roc_code_markup::markup::nodes::{self};
use roc_code_markup::slow_pool::MarkNodeId;

use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::{MissingParentSnafu, UnexpectedASTNodeSnafu};
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;

pub fn start_new_list(ed_model: &mut EdModel) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos: _,
        curr_mark_node_id: _,
        curr_mark_node,
        parent_id_opt: _,
        ast_node_id,
    } = get_node_context(ed_model)?;

    let is_blank_node = curr_mark_node.is_blank();

    let expr2_node = Expr2::List {
        elem_var: ed_model.module.env.var_store.fresh(),
        elems: PoolVec::empty(ed_model.module.env.pool),
    };

    ed_model
        .module
        .env
        .pool
        .set(ast_node_id.to_expr_id()?, expr2_node);

    if is_blank_node {
        ed_model.simple_move_carets_right(nodes::LEFT_SQUARE_BR.len());

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
        old_caret_pos: _,
        curr_mark_node_id,
        curr_mark_node: _,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(ed_model)?;

    let trip_result: EdResult<(ExprId, ExprId, MarkNodeId)> = if let Some(parent_id) = parent_id_opt
    {
        let list_ast_node_id = ed_model.mark_id_ast_id_map.get(parent_id)?;
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
            _ => UnexpectedASTNodeSnafu {
                required_node_type: "List".to_string(),
                encountered_node_type: ast_node_to_string(ast_node_id, ed_model.module.env.pool),
            }
            .fail(),
        }
    } else {
        MissingParentSnafu {
            node_id: curr_mark_node_id,
        }
        .fail()
    };

    let (blank_elt_id, list_ast_node_id, _parent_id) = trip_result?;

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
        _ => UnexpectedASTNodeSnafu {
            required_node_type: "List".to_string(),
            encountered_node_type: ast_node_to_string(ast_node_id, ed_model.module.env.pool),
        }
        .fail(),
    }?;

    if new_child_index > 1 {
        ed_model.simple_move_carets_right(nodes::COMMA.len());
    }

    Ok(InputOutcome::Accepted)
}
