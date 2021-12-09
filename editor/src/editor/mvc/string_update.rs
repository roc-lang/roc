use roc_ast::lang::core::expr::expr2::ArrString;
use roc_ast::lang::core::expr::expr2::Expr2;
use roc_ast::lang::core::str::update_str_expr;
use roc_ast::mem_pool::pool_str::PoolStr;
use roc_code_markup::markup::attribute::Attributes;
use roc_code_markup::markup::nodes;
use roc_code_markup::markup::nodes::MarkupNode;
use roc_code_markup::syntax_highlight::HighlightStyle;

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
        curr_mark_node: _,
        parent_id_opt: _,
        ast_node_id,
    } = get_node_context(ed_model)?;

    let new_input = &new_char.to_string();

    // update markup
    let curr_mark_node_mut = ed_model.mark_node_pool.get_mut(curr_mark_node_id);
    let content_str_mut = curr_mark_node_mut.get_content_mut()?;
    let node_caret_offset = ed_model
        .grid_node_map
        .get_offset_to_node_id(old_caret_pos, curr_mark_node_id)?;

    if node_caret_offset != 0 && node_caret_offset < content_str_mut.len() {
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

        content_str_mut.insert_str(node_caret_offset, new_input);

        // update GridNodeMap and CodeLines
        EdModel::insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            new_input,
            curr_mark_node_id,
            &mut ed_model.grid_node_map,
            &mut ed_model.code_lines,
        )?;

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
        curr_mark_node: _,
        parent_id_opt: _,
        ast_node_id,
    } = get_node_context(ed_model)?;

    // update markup
    let curr_mark_node_mut = ed_model.mark_node_pool.get_mut(curr_mark_node_id);
    let content_str_mut = curr_mark_node_mut.get_content_mut()?;
    let node_caret_offset = ed_model
        .grid_node_map
        .get_offset_to_node_id(old_caret_pos, curr_mark_node_id)?;

    if node_caret_offset != 0 && node_caret_offset < content_str_mut.len() {
        content_str_mut.insert(node_caret_offset, new_char);

        // update GridNodeMap and CodeLines
        EdModel::insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            &new_char.to_string(),
            curr_mark_node_id,
            &mut ed_model.grid_node_map,
            &mut ed_model.code_lines,
        )?;

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
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(ed_model)?;

    if curr_mark_node.is_blank() {
        let new_expr2_node = Expr2::SmallStr(arrayvec::ArrayString::new());
        let curr_mark_node_nls = curr_mark_node.get_newlines_at_end();

        ed_model
            .module
            .env
            .pool
            .set(ast_node_id.to_expr_id()?, new_expr2_node);

        let new_string_node = MarkupNode::Text {
            content: nodes::STRING_QUOTES.to_owned(),
            ast_node_id,
            syn_high_style: HighlightStyle::String,
            attributes: Attributes::default(),
            parent_id_opt,
            newlines_at_end: curr_mark_node_nls,
        };

        ed_model
            .mark_node_pool
            .replace_node(curr_mark_node_id, new_string_node);

        // remove data corresponding to Blank node
        ed_model.del_blank_expr_node(old_caret_pos)?;

        // update GridNodeMap and CodeLines
        EdModel::insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            nodes::STRING_QUOTES,
            curr_mark_node_id,
            &mut ed_model.grid_node_map,
            &mut ed_model.code_lines,
        )?;

        ed_model.simple_move_carets_right(1);

        Ok(InputOutcome::Accepted)
    } else {
        Ok(InputOutcome::Ignored)
    }
}
