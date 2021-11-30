use roc_ast::lang::{
    core::{ast::ASTNodeId, def::def2::Def2, expr::expr2::Expr2},
    env::Env,
};
use roc_code_markup::{
    markup::{
        attribute::Attributes,
        common_nodes::{new_blank_mn_w_nls, new_equals_mn},
        nodes::{set_parent_for_all, MarkupNode},
    },
    slow_pool::{MarkNodeId, SlowPool},
    syntax_highlight::HighlightStyle,
};
use roc_module::symbol::IdentId;

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

pub fn tld_mark_node<'a>(
    identifier_id: IdentId,
    expr_mark_node_id: MarkNodeId,
    ast_node_id: ASTNodeId,
    mark_node_pool: &mut SlowPool,
    env: &Env<'a>,
) -> EdResult<MarkupNode> {
    let val_name = env.ident_ids.get_name_str_res(identifier_id)?;

    let val_name_mn = MarkupNode::Text {
        content: val_name.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Value,
        attributes: Attributes::default(),
        parent_id_opt: None,
        newlines_at_end: 0,
    };

    let val_name_mn_id = mark_node_pool.add(val_name_mn);

    let equals_mn_id = mark_node_pool.add(new_equals_mn(ast_node_id, None));

    let full_let_node = MarkupNode::Nested {
        ast_node_id,
        children_ids: vec![val_name_mn_id, equals_mn_id, expr_mark_node_id],
        parent_id_opt: None,
        newlines_at_end: 2,
    };

    Ok(full_let_node)
}

pub fn start_new_tld_value(ed_model: &mut EdModel, new_char: &char) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node: _,
        parent_id_opt: _,
        ast_node_id,
    } = get_node_context(ed_model)?;

    let val_expr_node = Expr2::Blank;
    let val_expr_id = ed_model.module.env.pool.add(val_expr_node);

    let val_expr_mn = new_blank_mn_w_nls(ASTNodeId::AExprId(val_expr_id), None, 0);
    let val_expr_mn_id = ed_model.mark_node_pool.add(val_expr_mn);

    let val_name_string = new_char.to_string();

    let ident_id = ed_model
        .module
        .env
        .ident_ids
        .add(val_name_string.clone().into());

    let module_ident_ids_opt = ed_model
        .loaded_module
        .interns
        .all_ident_ids
        .get_mut(&ed_model.module.env.home);

    if let Some(module_ident_ids_ref) = module_ident_ids_opt {
        // this might create different IdentId for interns and env.ident_ids which may be a problem
        module_ident_ids_ref.add(val_name_string.into());
    } else {
        KeyNotFound {
            key_str: format!("{:?}", ed_model.module.env.home),
        }
        .fail()?
    }

    let tld_mark_node = tld_mark_node(
        ident_id,
        val_expr_mn_id,
        ast_node_id,
        &mut ed_model.mark_node_pool,
        &ed_model.module.env,
    )?;

    let new_ast_node = Def2::ValueDef {
        identifier_id: ident_id,
        expr_id: val_expr_id,
    };

    ed_model
        .module
        .env
        .pool
        .set(ast_node_id.to_def_id()?, new_ast_node);

    ed_model
        .mark_node_pool
        .replace_node(curr_mark_node_id, tld_mark_node);

    set_parent_for_all(curr_mark_node_id, &mut ed_model.mark_node_pool);

    // remove data corresponding to old Blank node
    ed_model.del_line(old_caret_pos.line + 1)?;
    ed_model.del_line(old_caret_pos.line)?;

    let char_len = 1;
    ed_model.simple_move_carets_right(char_len);

    let mut curr_line = old_caret_pos.line;
    let mut curr_column = old_caret_pos.column;

    EdModel::insert_mark_node_between_line(
        &mut curr_line,
        &mut curr_column,
        curr_mark_node_id,
        &mut ed_model.grid_node_map,
        &mut ed_model.code_lines,
        &ed_model.mark_node_pool,
    )?;

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
        let val_name_mn_mut = ed_model.mark_node_pool.get_mut(val_name_mn_id);
        let content_str_mut = val_name_mn_mut.get_content_mut()?;

        let old_val_name = content_str_mut.clone();

        let node_caret_offset = ed_model
            .grid_node_map
            .get_offset_to_node_id(old_caret_pos, val_name_mn_id)?;

        if node_caret_offset <= content_str_mut.len() {
            content_str_mut.insert(node_caret_offset, *new_char);

            let update_val_name_res = ed_model
                .module
                .env
                .ident_ids
                .update_key(&old_val_name, content_str_mut);

            if let Err(err_str) = update_val_name_res {
                FailedToUpdateIdentIdName { err_str }.fail()?;
            }

            EdModel::insert_between_line(
                old_caret_pos.line,
                old_caret_pos.column,
                &new_char.to_string(),
                val_name_mn_id,
                &mut ed_model.grid_node_map,
                &mut ed_model.code_lines,
            )?;

            ed_model.simple_move_caret_right(old_caret_pos, 1);

            Ok(InputOutcome::Accepted)
        } else {
            Ok(InputOutcome::Ignored)
        }
    } else {
        Ok(InputOutcome::Ignored)
    }
}
