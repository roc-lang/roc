use roc_ast::lang::core::{def::def2::Def2, expr::expr2::Expr2};
use roc_code_markup::slow_pool::MarkNodeId;

use crate::{
    editor::ed_error::{EdResult, FailedToUpdateIdentIdName, KeyNotFound},
    ui::text::text_pos::TextPos,
};

use super::{
    app_update::InputOutcome,
    ed_model::EdModel,
    ed_update::{get_node_context, NodeContext},
};

// Top Level Defined Value. example: `main = "Hello, World!"`
pub fn start_new_tld_value(ed_model: &mut EdModel, new_char: &char) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos: _,
        curr_mark_node_id: _,
        curr_mark_node: _,
        parent_id_opt: _,
        ast_node_id,
    } = get_node_context(ed_model)?;

    // create new blank >> m = Blank
    let val_expr_node = Expr2::Blank;
    let val_expr_id = ed_model.module.env.pool.add(val_expr_node);

    let ident_str = new_char.to_string();
    let ident_id = ed_model.module.env.ident_ids.add_str(&ident_str);

    let module_ident_ids_opt = ed_model
        .loaded_module
        .interns
        .all_ident_ids
        .get_mut(&ed_model.module.env.home);

    if let Some(module_ident_ids_ref) = module_ident_ids_opt {
        // this might create different IdentId for interns and env.ident_ids which may be a problem
        module_ident_ids_ref.add_str(&ident_str);
    } else {
        KeyNotFound {
            key_str: format!("{:?}", ed_model.module.env.home),
        }
        .fail()?
    }

    let new_ast_node = Def2::ValueDef {
        identifier_id: ident_id,
        expr_id: val_expr_id,
    };

    ed_model
        .module
        .env
        .pool
        .set(ast_node_id.to_def_id()?, new_ast_node);

    let char_len = 1;
    ed_model.simple_move_carets_right(char_len);

    Ok(InputOutcome::Accepted)
}

pub fn update_tld_val_name(
    val_name_mn_id: MarkNodeId,
    old_caret_pos: TextPos,
    ed_model: &mut EdModel,
    new_char: &char,
) -> EdResult<InputOutcome> {
    if new_char.is_ascii_alphanumeric() {
        // update markup
        let val_name_mn = ed_model.mark_node_pool.get(val_name_mn_id);
        let mut val_name_str = val_name_mn.get_content();

        let old_val_name = val_name_str.clone();

        let node_caret_offset = ed_model
            .grid_node_map
            .get_offset_to_node_id(old_caret_pos, val_name_mn_id)?;

        if node_caret_offset <= val_name_str.len() {
            val_name_str.insert(node_caret_offset, *new_char);

            let update_val_name_res = ed_model
                .module
                .env
                .ident_ids
                .update_key(&old_val_name, &val_name_str);

            if let Err(err_str) = update_val_name_res {
                FailedToUpdateIdentIdName { err_str }.fail()?;
            }

            ed_model.simple_move_caret_right(old_caret_pos, 1);

            Ok(InputOutcome::Accepted)
        } else {
            Ok(InputOutcome::Ignored)
        }
    } else {
        Ok(InputOutcome::Ignored)
    }
}
