use roc_ast::lang::core::ast::ASTNodeId;
use roc_ast::lang::core::expr::expr2::Expr2;
use roc_ast::lang::core::pattern::Pattern2;
use roc_ast::lang::core::val_def::ValueDef;
use roc_code_markup::markup::attribute::Attributes;
use roc_code_markup::markup::common_nodes::new_blank_mn_w_nls;
use roc_code_markup::markup::common_nodes::new_equals_mn;
use roc_code_markup::markup::nodes::MarkupNode;
use roc_code_markup::syntax_highlight::HighlightStyle;
use roc_module::symbol::Symbol;

use crate::editor::ed_error::EdResult;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;

pub fn start_new_let_value(ed_model: &mut EdModel, new_char: &char) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(ed_model)?;

    let is_blank_node = curr_mark_node.is_blank();
    let curr_mark_node_nls = curr_mark_node.get_newlines_at_end();

    let val_name_string = new_char.to_string();
    // safe unwrap because our ArrString has a 30B capacity
    let val_expr2_node = Expr2::Blank;
    let val_expr_id = ed_model.module.env.pool.add(val_expr2_node);

    let ident_id = ed_model
        .module
        .env
        .ident_ids
        .add(val_name_string.clone().into());
    let var_symbol = Symbol::new(ed_model.module.env.home, ident_id);
    let body = Expr2::Var(var_symbol);
    let body_id = ed_model.module.env.pool.add(body);

    let pattern = Pattern2::Identifier(var_symbol);
    let pattern_id = ed_model.module.env.pool.add(pattern);

    let value_def = ValueDef::NoAnnotation {
        pattern_id,
        expr_id: val_expr_id,
        expr_var: ed_model.module.env.var_store.fresh(),
    };
    let def_id = ed_model.module.env.pool.add(value_def);

    let expr2_node = Expr2::LetValue {
        def_id,
        body_id,
        body_var: ed_model.module.env.var_store.fresh(),
    };

    ed_model
        .module
        .env
        .pool
        .set(ast_node_id.to_expr_id()?, expr2_node);

    let val_name_mark_node = MarkupNode::Text {
        content: val_name_string,
        ast_node_id,
        syn_high_style: HighlightStyle::Value,
        attributes: Attributes::default(),
        parent_id_opt: Some(curr_mark_node_id),
        newlines_at_end: curr_mark_node_nls,
    };

    let val_name_mn_id = ed_model.add_mark_node(val_name_mark_node);

    let equals_mn_id = ed_model.add_mark_node(new_equals_mn(ast_node_id, Some(curr_mark_node_id)));

    let body_mn_id = ed_model.add_mark_node(new_blank_mn_w_nls(
        ASTNodeId::AExprId(val_expr_id),
        Some(curr_mark_node_id),
        1,
    ));

    let val_mark_node = MarkupNode::Nested {
        ast_node_id,
        children_ids: vec![val_name_mn_id, equals_mn_id, body_mn_id],
        parent_id_opt,
        newlines_at_end: 1,
    };

    if is_blank_node {
        ed_model
            .mark_node_pool
            .replace_node(curr_mark_node_id, val_mark_node);

        // remove data corresponding to Blank node
        ed_model.del_blank_expr_node(old_caret_pos)?;

        let char_len = 1;
        ed_model.simple_move_carets_right(char_len);

        // update GridNodeMap and CodeLines
        ed_model.insert_all_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            &[val_name_mn_id, equals_mn_id, body_mn_id],
        )?;

        Ok(InputOutcome::Accepted)
    } else {
        Ok(InputOutcome::Ignored)
    }
}

// TODO reenable this for updating non-top level value defs
/*
pub fn update_let_value(
    val_name_mn_id: MarkNodeId,
    def_id: NodeId<ValueDef>,
    body_id: NodeId<Expr2>,
    ed_model: &mut EdModel,
    new_char: &char,
) -> EdResult<InputOutcome> {
    if new_char.is_ascii_alphanumeric() {
        let old_caret_pos = ed_model.get_caret();

        // update markup
        let val_name_mn_mut = ed_model.mark_node_pool.get_mut(val_name_mn_id);
        let content_str_mut = val_name_mn_mut.get_content_mut()?;

        let old_val_name = content_str_mut.clone();

        let node_caret_offset = ed_model
            .grid_node_map
            .get_offset_to_node_id(old_caret_pos, val_name_mn_id)?;

        if node_caret_offset <= content_str_mut.len() {
            content_str_mut.insert(node_caret_offset, *new_char);

            // update ast
            let value_def = ed_model.module.env.pool.get(def_id);
            let value_ident_pattern_id = value_def.get_pattern_id();

            // TODO no unwrap
            let ident_id = ed_model
                .module
                .env
                .ident_ids
                .update_key(&old_val_name, content_str_mut)
                .unwrap();

            let new_var_symbol = Symbol::new(ed_model.module.env.home, ident_id);

            ed_model
                .module
                .env
                .pool
                .set(value_ident_pattern_id, Pattern2::Identifier(new_var_symbol));

            ed_model
                .module
                .env
                .pool
                .set(body_id, Expr2::Var(new_var_symbol));

            // update GridNodeMap and CodeLines
            ed_model.insert_between_line(
                old_caret_pos.line,
                old_caret_pos.column,
                &new_char.to_string(),
                val_name_mn_id,
            )?;

            // update caret
            ed_model.simple_move_carets_right(1);

            Ok(InputOutcome::Accepted)
        } else {
            Ok(InputOutcome::Ignored)
        }
    } else {
        Ok(InputOutcome::Ignored)
    }
}
*/
