use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::MissingParent;
use crate::editor::ed_error::RecordWithoutFields;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;
use crate::editor::util::index_of;
use crate::ui::text::text_pos::TextPos;
use roc_ast::lang::core::ast::ASTNodeId;
use roc_ast::lang::core::expr::expr2::Expr2;
use roc_ast::lang::core::expr::expr2::ExprId;
use roc_ast::lang::core::expr::record_field::RecordField;
use roc_ast::mem_pool::pool_str::PoolStr;
use roc_ast::mem_pool::pool_vec::PoolVec;
use roc_code_markup::markup::attribute::Attributes;
use roc_code_markup::markup::common_nodes::new_blank_mn;
use roc_code_markup::markup::common_nodes::new_left_accolade_mn;
use roc_code_markup::markup::common_nodes::new_right_accolade_mn;
use roc_code_markup::markup::nodes;
use roc_code_markup::markup::nodes::MarkupNode;
use roc_code_markup::slow_pool::MarkNodeId;
use roc_code_markup::syntax_highlight::HighlightStyle;
use snafu::OptionExt;

pub fn start_new_record(ed_model: &mut EdModel) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(ed_model)?;

    let is_blank_node = curr_mark_node.is_blank();
    let curr_mark_node_nls = curr_mark_node.get_newlines_at_end();

    let ast_pool = &mut ed_model.module.env.pool;
    let expr2_node = Expr2::EmptyRecord;

    ast_pool.set(ast_node_id.to_expr_id()?, expr2_node);

    let left_bracket_node_id = ed_model.add_mark_node(new_left_accolade_mn(
        ast_node_id.to_expr_id()?,
        Some(curr_mark_node_id),
    ));

    let right_bracket_node_id = ed_model.add_mark_node(new_right_accolade_mn(
        ast_node_id.to_expr_id()?,
        Some(curr_mark_node_id),
    ));

    let nested_node = MarkupNode::Nested {
        ast_node_id,
        children_ids: vec![left_bracket_node_id, right_bracket_node_id],
        parent_id_opt,
        newlines_at_end: curr_mark_node_nls,
    };

    if is_blank_node {
        ed_model
            .mark_node_pool
            .replace_node(curr_mark_node_id, nested_node);

        ed_model.del_blank_expr_node(old_caret_pos)?;

        ed_model.simple_move_carets_right(nodes::LEFT_ACCOLADE.len());

        // update GridNodeMap and CodeLines
        ed_model.insert_all_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            &[left_bracket_node_id, right_bracket_node_id],
        )?;

        Ok(InputOutcome::Accepted)
    } else {
        Ok(InputOutcome::Ignored)
    }
}

pub fn update_empty_record(
    new_input: &str,
    prev_mark_node_id: MarkNodeId,
    sibling_ids: Vec<MarkNodeId>,
    ed_model: &mut EdModel,
) -> EdResult<InputOutcome> {
    let mut input_chars = new_input.chars();

    if input_chars.all(|ch| ch.is_ascii_alphabetic())
        && input_chars.all(|ch| ch.is_ascii_lowercase())
    {
        let prev_mark_node = ed_model.mark_node_pool.get(prev_mark_node_id);

        let NodeContext {
            old_caret_pos,
            curr_mark_node_id,
            curr_mark_node,
            parent_id_opt,
            ast_node_id,
        } = get_node_context(ed_model)?;

        if prev_mark_node.get_content() == nodes::LEFT_ACCOLADE
            && curr_mark_node.get_content() == nodes::RIGHT_ACCOLADE
        {
            // update AST
            let record_var = ed_model.module.env.var_store.fresh();
            let field_name = PoolStr::new(new_input, ed_model.module.env.pool);
            let field_var = ed_model.module.env.var_store.fresh();
            //TODO actually check if field_str belongs to a previously defined variable
            let first_field = RecordField::InvalidLabelOnly(field_name, field_var);

            let fields = PoolVec::new(vec![first_field].into_iter(), ed_model.module.env.pool);

            let new_ast_node = Expr2::Record { record_var, fields };

            ed_model
                .module
                .env
                .pool
                .set(ast_node_id.to_expr_id()?, new_ast_node);

            // update Markup

            let record_field_node = MarkupNode::Text {
                content: new_input.to_owned(),
                ast_node_id,
                syn_high_style: HighlightStyle::RecordField,
                attributes: Attributes::default(),
                parent_id_opt,
                newlines_at_end: 0,
            };

            let record_field_node_id = ed_model.add_mark_node(record_field_node);

            if let Some(parent_id) = parent_id_opt {
                let parent = ed_model.mark_node_pool.get_mut(parent_id);

                let new_child_index = index_of(curr_mark_node_id, &sibling_ids)?;

                parent.add_child_at_index(new_child_index, record_field_node_id)?;
            } else {
                MissingParent {
                    node_id: curr_mark_node_id,
                }
                .fail()?
            }

            // update caret
            ed_model.simple_move_carets_right(1);

            // update GridNodeMap and CodeLines
            EdModel::insert_between_line(
                old_caret_pos.line,
                old_caret_pos.column,
                new_input,
                record_field_node_id,
                &mut ed_model.grid_node_map,
                &mut ed_model.code_lines,
            )?;

            Ok(InputOutcome::Accepted)
        } else {
            Ok(InputOutcome::Ignored)
        }
    } else {
        Ok(InputOutcome::Ignored)
    }
}

pub fn update_record_colon(
    ed_model: &mut EdModel,
    record_ast_node_id: ExprId,
) -> EdResult<InputOutcome> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(ed_model)?;
    if let Some(parent_id) = parent_id_opt {
        let curr_ast_node = ed_model.module.env.pool.get(ast_node_id.to_expr_id()?);

        let prev_mark_node_id_opt = ed_model.get_prev_mark_node_id()?;
        if let Some(prev_mark_node_id) = prev_mark_node_id_opt {
            let prev_mark_node = ed_model.mark_node_pool.get(prev_mark_node_id);

            match prev_mark_node.get_ast_node_id() {
                ASTNodeId::ADefId(_) => Ok(InputOutcome::Ignored),
                ASTNodeId::AExprId(prev_expr_id) => {
                    let prev_expr = ed_model.module.env.pool.get(prev_expr_id);

                    // current and prev node should always point to record when in valid position to add ':'
                    if matches!(prev_expr, Expr2::Record { .. })
                        && matches!(curr_ast_node, Expr2::Record { .. })
                    {
                        let sibling_ids = curr_mark_node.get_sibling_ids(&ed_model.mark_node_pool);

                        let new_child_index = index_of(curr_mark_node_id, &sibling_ids)?;

                        let ast_node_ref = ed_model.module.env.pool.get(record_ast_node_id);

                        match ast_node_ref {
                            Expr2::Record {
                                record_var: _,
                                fields,
                            } => {
                                if ed_model.node_exists_at_caret() {
                                    let next_mark_node_id =
                                        ed_model.grid_node_map.get_id_at_row_col(old_caret_pos)?;
                                    let next_mark_node =
                                        ed_model.mark_node_pool.get(next_mark_node_id);
                                    if next_mark_node.get_content() == nodes::RIGHT_ACCOLADE {
                                        // update AST node
                                        let new_field_val = Expr2::Blank;
                                        let new_field_val_id =
                                            ed_model.module.env.pool.add(new_field_val);

                                        let first_field_mut = fields
                                            .iter_mut(ed_model.module.env.pool)
                                            .next()
                                            .with_context(|| RecordWithoutFields {})?;

                                        *first_field_mut = RecordField::LabeledValue(
                                            *first_field_mut.get_record_field_pool_str(),
                                            *first_field_mut.get_record_field_var(),
                                            new_field_val_id,
                                        );

                                        // update Markup
                                        let record_colon = nodes::COLON;

                                        let record_colon_node = MarkupNode::Text {
                                            content: record_colon.to_owned(),
                                            ast_node_id: ASTNodeId::AExprId(record_ast_node_id),
                                            syn_high_style: HighlightStyle::Operator,
                                            attributes: Attributes::default(),
                                            parent_id_opt: Some(parent_id),
                                            newlines_at_end: 0,
                                        };

                                        let record_colon_node_id =
                                            ed_model.add_mark_node(record_colon_node);
                                        ed_model
                                            .mark_node_pool
                                            .get_mut(parent_id)
                                            .add_child_at_index(
                                                new_child_index,
                                                record_colon_node_id,
                                            )?;

                                        let record_blank_node_id =
                                            ed_model.add_mark_node(new_blank_mn(
                                                ASTNodeId::AExprId(new_field_val_id),
                                                Some(parent_id),
                                            ));

                                        ed_model
                                            .mark_node_pool
                                            .get_mut(parent_id)
                                            .add_child_at_index(
                                                new_child_index + 1,
                                                record_blank_node_id,
                                            )?;

                                        // update caret
                                        ed_model.simple_move_carets_right(record_colon.len());

                                        // update GridNodeMap and CodeLines
                                        ed_model.insert_all_between_line(
                                            old_caret_pos.line,
                                            old_caret_pos.column,
                                            &[record_colon_node_id, record_blank_node_id],
                                        )?;

                                        Ok(InputOutcome::Accepted)
                                    } else {
                                        Ok(InputOutcome::Ignored)
                                    }
                                } else {
                                    Ok(InputOutcome::Ignored)
                                }
                            }
                            _ => Ok(InputOutcome::Ignored),
                        }
                    } else {
                        Ok(InputOutcome::Ignored)
                    }
                }
            }
        } else {
            Ok(InputOutcome::Ignored)
        }
    } else {
        MissingParent {
            node_id: curr_mark_node_id,
        }
        .fail()
    }
}

pub fn update_record_field(
    new_input: &str,
    old_caret_pos: TextPos,
    curr_mark_node_id: MarkNodeId,
    record_fields: &PoolVec<RecordField>,
    ed_model: &mut EdModel,
) -> EdResult<InputOutcome> {
    // update MarkupNode
    let curr_mark_node_mut = ed_model.mark_node_pool.get_mut(curr_mark_node_id);
    let content_str_mut = curr_mark_node_mut.get_content_mut()?;
    let node_caret_offset = ed_model
        .grid_node_map
        .get_offset_to_node_id(old_caret_pos, curr_mark_node_id)?;

    if node_caret_offset == 0 {
        let first_char_opt = new_input.chars().next();
        let first_char_is_num = first_char_opt.unwrap_or('0').is_ascii_digit();

        // variable name can't start with number
        if first_char_is_num {
            return Ok(InputOutcome::Ignored);
        }
    }

    content_str_mut.insert_str(node_caret_offset, new_input);

    // update caret
    ed_model.simple_move_carets_right(new_input.len());

    // update GridNodeMap and CodeLines
    EdModel::insert_between_line(
        old_caret_pos.line,
        old_caret_pos.column,
        new_input,
        curr_mark_node_id,
        &mut ed_model.grid_node_map,
        &mut ed_model.code_lines,
    )?;

    // update AST Node
    let first_field = record_fields
        .iter(ed_model.module.env.pool)
        .next()
        .with_context(|| RecordWithoutFields {})?;

    let field_pool_str = first_field
        .get_record_field_pool_str()
        .as_str(ed_model.module.env.pool);

    let mut new_field_name = String::new();

    // -push old field name
    new_field_name.push_str(field_pool_str);
    new_field_name.insert_str(node_caret_offset, new_input);
    let new_field_pool_str = PoolStr::new(&new_field_name, ed_model.module.env.pool);

    let first_field_mut = record_fields
        .iter_mut(ed_model.module.env.pool)
        .next()
        .with_context(|| RecordWithoutFields {})?;

    let field_pool_str_mut = first_field_mut.get_record_field_pool_str_mut();
    *field_pool_str_mut = new_field_pool_str;

    // because borrow issues
    let first_field_b = record_fields
        .iter(ed_model.module.env.pool)
        .next()
        .with_context(|| RecordWithoutFields {})?;

    match first_field_b {
        RecordField::InvalidLabelOnly(_, _) => {
            // TODO check if label is now valid. If it is, return LabelOnly
        }
        RecordField::LabelOnly(_, _, _symbol) => {
            // TODO check if symbol is still valid. If not, return InvalidLabelOnly
        }
        RecordField::LabeledValue(_, _, field_val_id_ref) => {
            let field_val_id = *field_val_id_ref;
            let sub_expr2 = ed_model.module.env.pool.get(field_val_id);

            if let Expr2::InvalidLookup(_) = sub_expr2 {
                ed_model
                    .module
                    .env
                    .pool
                    .set(field_val_id, Expr2::InvalidLookup(new_field_pool_str));
            }
        }
    }

    Ok(InputOutcome::Accepted)
}
