use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::StringParseError;
use crate::editor::markup::attribute::Attributes;
use crate::editor::markup::common_nodes::new_blank_mn;
use crate::editor::markup::common_nodes::new_equals_mn;
use crate::editor::markup::nodes::MarkupNode;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::syntax_highlight::HighlightStyle;
use crate::lang::ast::ArrString;
use crate::lang::ast::{Expr2, ValueDef};
use crate::lang::ast::Expr2::SmallInt;
use crate::lang::ast::IntVal;
use crate::lang::ast::{IntStyle, IntVal::*};
use crate::lang::pool::PoolStr;
use crate::ui::text::lines::SelectableLines;


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
    let mut val_name_string_container = ArrString::try_from_str(val_name_string).unwrap();
    let val_name_expr2_node = 
        Expr2::SmallStr(
            val_name_string_container
        );
    let val_name_expr_id = ed_model.module.env.pool.add(val_name_expr2_node);

    let body_placeholder = Expr2::Blank;
    let body_id = ed_model.module.env.pool.add(body_placeholder);

    let value_def =
        ValueDef::NoAnnotation {
            pattern_id: Pattern2::Identifier(val_name_string),
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

    let val_mn_id = new_blank_mn(ast_node_id, Some(curr_mark_node_id), &mut ed_model.markup_node_pool);

    let val_mark_node = MarkupNode::Nested {
        ast_node_id,
        children_ids: vec![val_name_mn_id, equals_mn_id, val_mn_id],
        parent_id_opt,
    };

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
            &digit_char.to_string(),
            curr_mark_node_id,
        )?;

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

        let int_mark_node = ed_model.markup_node_pool.get_mut(int_mark_node_id);
        let int_ast_node_id = int_mark_node.get_ast_node_id();

        let content_str_mut = int_mark_node.get_content_mut()?;

        // 00, 01 are not valid ints
        if (content_str_mut == "0" && (node_caret_offset == 1 || *ch == '0'))
            || (*ch == '0' && node_caret_offset == 0)
        {
            Ok(InputOutcome::Ignored)
        } else {
            content_str_mut.insert(node_caret_offset, *ch);

            let content_str = int_mark_node.get_content()?;

            // update GridNodeMap and CodeLines
            ed_model.insert_between_line(
                old_caret_pos.line,
                old_caret_pos.column,
                &ch.to_string(),
                int_mark_node_id,
            )?;

            // update ast
            let new_pool_str = PoolStr::new(&content_str, ed_model.module.env.pool);
            let int_ast_node = ed_model.module.env.pool.get_mut(int_ast_node_id);
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
        Err(parse_err) => StringParseError {
            msg: format!("{:?}", parse_err),
        }
        .fail(),
    }
}
