use inlinable_string::InlinableString;
use roc_module::symbol::Symbol;

use crate::editor::ed_error::{EdResult};
use crate::editor::markup::attribute::Attributes;
use crate::editor::markup::common_nodes::new_blank_mn;
use crate::editor::markup::common_nodes::new_equals_mn;
use crate::editor::markup::nodes;
use crate::editor::markup::nodes::MarkupNode;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::syntax_highlight::HighlightStyle;
use crate::lang::ast::ArrString;
use crate::lang::ast::{Expr2, ValueDef, update_str_expr};
use crate::lang::pattern::Pattern2;
use crate::ui::text::lines::SelectableLines;
use std::iter::FromIterator;


pub fn start_new_let_value(ed_model: &mut EdModel, new_char: &char) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(&ed_model)?;

    let is_blank_node = curr_mark_node.is_blank();

    let val_name_string = new_char.to_string();
    // safe unwrap because our ArrString has a 30B capacity
    let val_name_string_container = ArrString::try_from_str(val_name_string.clone()).unwrap();
    let val_name_expr2_node = 
        Expr2::SmallStr(
            val_name_string_container
        );
    let val_name_expr_id = ed_model.module.env.pool.add(val_name_expr2_node);

    let body_placeholder = Expr2::Blank;
    let body_id = ed_model.module.env.pool.add(body_placeholder);

    let ident_string = InlinableString::from_iter(val_name_string.chars());
    let ident_id = ed_model.module.env.ident_ids.add(ident_string);

    let pattern = Pattern2::Identifier(Symbol::new(ed_model.module.env.home, ident_id));
    let pattern_id = ed_model.module.env.pool.add(pattern);

    let value_def =
        ValueDef::NoAnnotation {
            pattern_id,
            expr_id: val_name_expr_id,
            expr_var: ed_model.module.env.var_store.fresh()
        };
    let def_id = ed_model.module.env.pool.add(value_def);

    let expr2_node = Expr2::LetValue {
        def_id,
        body_id,
        body_var: ed_model.module.env.var_store.fresh(),
    };

    ed_model.module.env.pool.set(ast_node_id, expr2_node);

    let val_name_mark_node = MarkupNode::Text {
        content: val_name_string,
        ast_node_id,
        syn_high_style: HighlightStyle::Variable,
        attributes: Attributes::new(),
        parent_id_opt: Some(curr_mark_node_id),
    };

    let val_name_mn_id = ed_model.markup_node_pool.add(val_name_mark_node);

    let equals_mn_id = new_equals_mn(ast_node_id, Some(curr_mark_node_id), &mut ed_model.markup_node_pool);

    let body_mn_id = new_blank_mn(body_id, Some(curr_mark_node_id), &mut ed_model.markup_node_pool);

    let val_mark_node = MarkupNode::Nested {
        ast_node_id,
        children_ids: vec![val_name_mn_id, equals_mn_id, body_mn_id],
        parent_id_opt,
    };

    let child_nodes_content = val_mark_node.get_nested_content(&ed_model.markup_node_pool);

    if is_blank_node {
        ed_model
            .markup_node_pool
            .replace_node(curr_mark_node_id, val_mark_node);

        // remove data corresponding to Blank node
        ed_model.del_at_line(old_caret_pos.line, old_caret_pos.column)?;

        let char_len = 1;
        ed_model.simple_move_carets_right(char_len);

        // update GridNodeMap and CodeLines
        ed_model.insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            &child_nodes_content,
            curr_mark_node_id,
        )?;

        // for Blank node in body
        ed_model.insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column + child_nodes_content.len(),
            nodes::BLANK_PLACEHOLDER,
            body_mn_id,
        )?;

        Ok(InputOutcome::Accepted)
    } else {
        Ok(InputOutcome::Ignored)
    }
}

pub fn update_let_value(val_name_mn_id: MarkNodeId, ed_model: &mut EdModel, new_char: &char) -> EdResult<InputOutcome> {
    if new_char.is_ascii_alphanumeric() {
        let old_caret_pos = ed_model.get_caret();

        // update markup
        let val_name_mn_mut = ed_model.markup_node_pool.get_mut(val_name_mn_id);
        let content_str_mut = val_name_mn_mut.get_content_mut()?;
        let node_caret_offset = ed_model
            .grid_node_map
            .get_offset_to_node_id(old_caret_pos, val_name_mn_id)?;

        if node_caret_offset != 0 && node_caret_offset < content_str_mut.len() {
            content_str_mut.insert(node_caret_offset, *new_char);

            // update GridNodeMap and CodeLines
            ed_model.insert_between_line(
                old_caret_pos.line,
                old_caret_pos.column,
                &new_char.to_string(),
                val_name_mn_id,
            )?;

            // update caret
            ed_model.simple_move_carets_right(1);

            // update ast
            let let_value_ast_node_id = ed_model.markup_node_pool.get(val_name_mn_id).get_ast_node_id();
            update_str_expr(let_value_ast_node_id, *new_char, node_caret_offset, ed_model.module.env.pool)?;

            Ok(InputOutcome::Accepted)
        } else {
            Ok(InputOutcome::Ignored)
        }

    } else {
        Ok(InputOutcome::Ignored)
    }
}
