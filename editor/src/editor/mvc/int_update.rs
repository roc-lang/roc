
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::lang::ast::IntVal;
use crate::lang::ast::Expr2::SmallInt;
use crate::lang::ast::{IntStyle, IntVal::*};
use crate::lang::pool::PoolStr;
use crate::editor::ed_error::{EdResult};
use crate::editor::markup::nodes::MarkupNode;
use crate::editor::syntax_highlight::HighlightStyle;
use crate::editor::markup::attribute::Attributes;
use crate::ui::text::lines::SelectableLines;


// digit_char should be verified to be a digit before calling this function
pub fn start_new_int(ed_model: &mut EdModel, digit_char: &char) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(&ed_model)?;

    let is_blank_node = curr_mark_node.is_blank();

    let int_var = ed_model.module.env.var_store.fresh();

    let digit_string = digit_char.to_string();

    let expr2_node = 
        SmallInt {
            number: IntVal::U64(*digit_char as u64),  // TODO determine if u64 on wordlength of current arch, perhaps introduce Unknown(i64)
            var: int_var,   
            style: IntStyle::Decimal, 
            text: PoolStr::new(&digit_string, &mut ed_model.module.env.pool)   
        };

    ed_model.module.env.pool.set(ast_node_id, expr2_node);

    let int_node = MarkupNode::Text {
        content: digit_string,
        ast_node_id,
        syn_high_style: HighlightStyle::Number,
        attributes: Attributes::new(),
        parent_id_opt,
    };

    if is_blank_node {
        ed_model.markup_node_pool.replace_node(curr_mark_node_id, int_node);

        // remove data corresponding to Blank node
        ed_model.del_at_line(old_caret_pos.line, old_caret_pos.column)?;

        let char_len = 1;
        ed_model.simple_move_carets_right(char_len);

        // update GridNodeMap and CodeLines
        ed_model.insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            &digit_char.to_string(),
            curr_mark_node_id,
        )?;

        Ok(InputOutcome::Accepted)
    } else {
        Ok(InputOutcome::Ignored)
    }
}


// digit_char should be verified to be a digit before calling this function
pub fn update_int(ed_model: &mut EdModel, int_mark_node_id: MarkNodeId, digit_char: &char) -> EdResult<InputOutcome> {
    let old_caret_pos = ed_model.get_caret();

    let node_caret_offset = ed_model
        .grid_node_map
        .get_offset_to_node_id(old_caret_pos, int_mark_node_id)?;

    let int_mark_node = ed_model.markup_node_pool.get_mut(int_mark_node_id);
    let int_ast_node_id = int_mark_node.get_ast_node_id();

    let content_str_mut = int_mark_node.get_content_mut()?;
    content_str_mut.insert(node_caret_offset, *digit_char);

    let content_str = int_mark_node.get_content()?;

     // update GridNodeMap and CodeLines
     ed_model.insert_between_line(
        old_caret_pos.line,
        old_caret_pos.column,
        &digit_char.to_string(),
        int_mark_node_id,
    )?;

    // update ast
    let new_pool_str = PoolStr::new(&content_str, ed_model.module.env.pool);
    let int_ast_node = ed_model.module.env.pool.get_mut(int_ast_node_id);
    match int_ast_node {
        SmallInt{ number, text, .. } => {

            *number = match number {
                // TODO remove unwrap
                I64(_) => I64(content_str.parse::<i64>().unwrap()),
                U64(_) => U64(content_str.parse::<u64>().unwrap()),
                I32(_) => I32(content_str.parse::<i32>().unwrap()),
                U32(_) => U32(content_str.parse::<u32>().unwrap()),
                I16(_) => I16(content_str.parse::<i16>().unwrap()),
                U16(_) => U16(content_str.parse::<u16>().unwrap()),
                I8(_) => I8(content_str.parse::<i8>().unwrap()),
                U8(_) => U8(content_str.parse::<u8>().unwrap()),
            };
            
            *text = new_pool_str;
        }
        _ => unimplemented!("TODO implement updating this type of Number")
    }

    // update caret
    ed_model.simple_move_carets_right(1);

    Ok(InputOutcome::Accepted)
}