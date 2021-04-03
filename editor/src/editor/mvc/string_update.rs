use crate::editor::ed_error::EdResult;
use crate::editor::markup::attribute::Attributes;
use crate::editor::markup::nodes;
use crate::editor::markup::nodes::MarkupNode;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;
use crate::editor::syntax_highlight::HighlightStyle;
use crate::lang::ast::ArrString;
use crate::lang::ast::Expr2;
use crate::lang::pool::PoolStr;

pub fn update_small_string(
    new_char: &char,
    old_array_str: &ArrString,
    ed_model: &mut EdModel,
) -> EdResult<()> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node: _,
        parent_id_opt: _,
        ast_node_id,
    } = get_node_context(&ed_model)?;

    let new_input = &new_char.to_string();

    // update markup
    let curr_mark_node_mut = ed_model.markup_node_pool.get_mut(curr_mark_node_id);
    let content_str_mut = curr_mark_node_mut.get_content_mut()?;
    let node_caret_offset = ed_model
        .grid_node_map
        .get_offset_to_node_id(old_caret_pos, curr_mark_node_id)?;
    content_str_mut.insert_str(node_caret_offset, new_input);

    // update GridNodeMap and CodeLines
    ed_model.insert_between_line(
        old_caret_pos.line,
        old_caret_pos.column,
        new_input,
        curr_mark_node_id,
    )?;

    if old_array_str.len() < ArrString::capacity() {
        if let Expr2::SmallStr(ref mut mut_array_str) =
            ed_model.module.env.pool.get_mut(ast_node_id)
        {
            // safe because we checked the length
            unsafe {
                mut_array_str.push_unchecked(*new_char);
            }
        } else {
            unreachable!()
        }
    } else {
        let mut new_str = old_array_str.as_str().to_owned();
        new_str.push(*new_char);

        let new_ast_node = Expr2::Str(PoolStr::new(&new_str, ed_model.module.env.pool));

        ed_model.module.env.pool.set(ast_node_id, new_ast_node);
    }

    // update caret
    for _ in 0..new_input.len() {
        ed_model.simple_move_carets_right();
    }

    Ok(())
}

pub fn update_string(
    new_input: &str,
    old_pool_str: &PoolStr,
    ed_model: &mut EdModel,
) -> EdResult<()> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node: _,
        parent_id_opt: _,
        ast_node_id,
    } = get_node_context(&ed_model)?;

    // update markup
    let curr_mark_node_mut = ed_model.markup_node_pool.get_mut(curr_mark_node_id);
    let content_str_mut = curr_mark_node_mut.get_content_mut()?;
    let node_caret_offset = ed_model
        .grid_node_map
        .get_offset_to_node_id(old_caret_pos, curr_mark_node_id)?;
    content_str_mut.insert_str(node_caret_offset, new_input);

    // update GridNodeMap and CodeLines
    ed_model.insert_between_line(
        old_caret_pos.line,
        old_caret_pos.column,
        new_input,
        curr_mark_node_id,
    )?;

    // update ast
    let mut new_string = old_pool_str.as_str(ed_model.module.env.pool).to_owned();
    new_string.push_str(new_input);

    let new_pool_str = PoolStr::new(&new_string, &mut ed_model.module.env.pool);
    let new_ast_node = Expr2::Str(new_pool_str);

    ed_model.module.env.pool.set(ast_node_id, new_ast_node);

    // update caret
    ed_model.simple_move_carets_right();

    Ok(())
}

pub fn start_new_string(ed_model: &mut EdModel) -> EdResult<()> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(&ed_model)?;

    if curr_mark_node.is_blank() {
        let new_expr2_node = Expr2::SmallStr(arraystring::ArrayString::new());

        ed_model.module.env.pool.set(ast_node_id, new_expr2_node);

        let new_string_node = MarkupNode::Text {
            content: nodes::STRING_QUOTES.to_owned(),
            ast_node_id,
            syn_high_style: HighlightStyle::String,
            attributes: Attributes::new(),
            parent_id_opt,
        };

        ed_model
            .markup_node_pool
            .replace_node(curr_mark_node_id, new_string_node);

        // remove data corresponding to Blank node
        ed_model.del_at_line(old_caret_pos.line, old_caret_pos.column)?;

        // update GridNodeMap and CodeLines
        ed_model.insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            nodes::STRING_QUOTES,
            curr_mark_node_id,
        )?;

        ed_model.simple_move_carets_right();
    }

    Ok(())
}
