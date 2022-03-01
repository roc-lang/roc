use roc_ast::lang::core::expr::expr2::ArrString;
use roc_ast::lang::core::expr::expr2::Expr2;
use roc_ast::lang::core::str::update_str_expr;
use roc_ast::mem_pool::pool_str::PoolStr;

use crate::editor::ed_error::EdResult;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;

pub fn update_small_string(
    new_char: &char,
    old_array_str: &ArrString,
    ed_model: &mut EdModel,
) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt: _,
        ast_node_id,
    } = get_node_context(ed_model)?;

    let new_input = &new_char.to_string();

    let content_str = curr_mark_node.get_content();
    let node_caret_offset = ed_model
        .grid_node_map
        .get_offset_to_node_id(old_caret_pos, curr_mark_node_id)?;

    if node_caret_offset != 0 && node_caret_offset < content_str.len() {
        if old_array_str.len() < old_array_str.capacity() {
            if let Expr2::SmallStr(ref mut mut_array_str) =
                ed_model.module.env.pool.get_mut(ast_node_id.to_expr_id()?)
            {
                // safe because we checked the length
                mut_array_str.push(*new_char);
            } else {
                unreachable!()
            }
        } else {
            let mut new_str = old_array_str.as_str().to_owned();
            new_str.push(*new_char);

            let new_ast_node = Expr2::Str(PoolStr::new(&new_str, ed_model.module.env.pool));

            ed_model
                .module
                .env
                .pool
                .set(ast_node_id.to_expr_id()?, new_ast_node);
        }

        // update caret
        ed_model.simple_move_carets_right(new_input.len());

        Ok(InputOutcome::Accepted)
    } else {
        Ok(InputOutcome::Ignored)
    }
}

pub fn update_string(new_char: char, ed_model: &mut EdModel) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt: _,
        ast_node_id,
    } = get_node_context(ed_model)?;

    let content_str = curr_mark_node.get_content();
    let node_caret_offset = ed_model
        .grid_node_map
        .get_offset_to_node_id(old_caret_pos, curr_mark_node_id)?;

    if node_caret_offset != 0 && node_caret_offset < content_str.len() {
        // update ast
        update_str_expr(
            ast_node_id.to_expr_id()?,
            new_char,
            node_caret_offset - 1, // -1 because offset was calculated with quotes
            ed_model.module.env.pool,
        )?;

        // update caret
        ed_model.simple_move_carets_right(1);

        Ok(InputOutcome::Accepted)
    } else {
        Ok(InputOutcome::Ignored)
    }
}

pub fn start_new_string(ed_model: &mut EdModel) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos: _,
        curr_mark_node_id: _,
        curr_mark_node,
        parent_id_opt: _,
        ast_node_id,
    } = get_node_context(ed_model)?;

    if curr_mark_node.is_blank() {
        let new_expr2_node = Expr2::SmallStr(arrayvec::ArrayString::new());

        ed_model
            .module
            .env
            .pool
            .set(ast_node_id.to_expr_id()?, new_expr2_node);

        ed_model.simple_move_carets_right(1);

        Ok(InputOutcome::Accepted)
    } else {
        Ok(InputOutcome::Ignored)
    }
}
