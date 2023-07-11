use roc_ast::lang::core::expr::expr2::Expr2::SmallInt;
use roc_ast::lang::core::expr::expr2::IntStyle;
use roc_ast::lang::core::expr::expr2::IntVal;
use roc_ast::mem_pool::pool_str::PoolStr;
use roc_code_markup::slow_pool::MarkNodeId;

use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::StringParseSnafu;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;
use crate::ui::text::lines::SelectableLines;

// digit_char should be verified to be a digit before calling this function
pub fn start_new_int(ed_model: &mut EdModel, digit_char: &char) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos: _,
        curr_mark_node_id: _,
        curr_mark_node,
        parent_id_opt: _,
        ast_node_id,
    } = get_node_context(ed_model)?;

    let is_blank_node = curr_mark_node.is_blank();

    let int_var = ed_model.module.env.var_store.fresh();

    let digit_string = digit_char.to_string();

    let expr2_node = SmallInt {
        number: IntVal::U64(*digit_char as u64), // TODO determine if u64 on wordlength of current arch, perhaps introduce Unknown(i64)
        var: int_var,
        style: IntStyle::Decimal,
        text: PoolStr::new(&digit_string, ed_model.module.env.pool),
    };

    ed_model
        .module
        .env
        .pool
        .set(ast_node_id.to_expr_id()?, expr2_node);

    if is_blank_node {
        let char_len = 1;
        ed_model.simple_move_carets_right(char_len);

        Ok(InputOutcome::Accepted)
    } else {
        Ok(InputOutcome::Ignored)
    }
}

// TODO check if new int needs more than e.g. 64 bits
pub fn update_int(
    ed_model: &mut EdModel,
    int_mark_node_id: MarkNodeId,
    ch: &char,
) -> EdResult<InputOutcome> {
    if ch.is_ascii_digit() {
        let old_caret_pos = ed_model.get_caret();

        let node_caret_offset = ed_model
            .grid_node_map
            .get_offset_to_node_id(old_caret_pos, int_mark_node_id)?;

        let int_mark_node = ed_model.mark_node_pool.get_mut(int_mark_node_id);
        let int_ast_node_id = ed_model.mark_id_ast_id_map.get(int_mark_node_id)?;

        let content_str_mut = int_mark_node.get_content_mut()?;

        // 00, 01 are not valid ints
        if (content_str_mut == "0" && (node_caret_offset == 1 || *ch == '0'))
            || (*ch == '0' && node_caret_offset == 0)
        {
            Ok(InputOutcome::Ignored)
        } else {
            content_str_mut.insert(node_caret_offset, *ch);

            let content_str = int_mark_node.get_content();

            // update ast
            let new_pool_str = PoolStr::new(&content_str, ed_model.module.env.pool);
            let int_ast_node = ed_model
                .module
                .env
                .pool
                .get_mut(int_ast_node_id.to_expr_id()?);
            match int_ast_node {
                SmallInt { number, text, .. } => {
                    update_small_int_num(number, &content_str)?;

                    *text = new_pool_str;
                }
                _ => unimplemented!("TODO implement updating this type of Number"),
            }

            // update caret
            ed_model.simple_move_carets_right(1);

            Ok(InputOutcome::Accepted)
        }
    } else {
        Ok(InputOutcome::Ignored)
    }
}

fn update_small_int_num(number: &mut IntVal, updated_str: &str) -> EdResult<()> {
    use IntVal::*;

    *number = match number {
        I64(_) => I64(check_parse_res(updated_str.parse::<i64>())?),
        U64(_) => U64(check_parse_res(updated_str.parse::<u64>())?),
        I32(_) => I32(check_parse_res(updated_str.parse::<i32>())?),
        U32(_) => U32(check_parse_res(updated_str.parse::<u32>())?),
        I16(_) => I16(check_parse_res(updated_str.parse::<i16>())?),
        U16(_) => U16(check_parse_res(updated_str.parse::<u16>())?),
        I8(_) => I8(check_parse_res(updated_str.parse::<i8>())?),
        U8(_) => U8(check_parse_res(updated_str.parse::<u8>())?),
    };

    Ok(())
}

fn check_parse_res<T, E: std::fmt::Debug>(parse_res: Result<T, E>) -> EdResult<T> {
    match parse_res {
        Ok(some_type) => Ok(some_type),
        Err(parse_err) => StringParseSnafu {
            msg: format!("{parse_err:?}"),
        }
        .fail(),
    }
}
