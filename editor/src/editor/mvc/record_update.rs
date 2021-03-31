use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::MissingParent;
use crate::editor::ed_error::RecordWithoutFields;
use crate::editor::markup::attribute::Attributes;
use crate::editor::markup::nodes;
use crate::editor::markup::nodes::MarkupNode;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_update::get_node_context;
use crate::editor::mvc::ed_update::NodeContext;
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::syntax_highlight::HighlightStyle;
use crate::editor::util::index_of;
use crate::lang::ast::Expr2;
use crate::lang::pool::NodeId;
use crate::lang::pool::PoolStr;
use crate::lang::pool::PoolVec;
use crate::ui::text::text_pos::TextPos;
use roc_types::subs::Variable;
use snafu::OptionExt;

pub fn start_new_record(ed_model: &mut EdModel) -> EdResult<()> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(&ed_model)?;

    let is_blank_node = curr_mark_node.is_blank();

    let ast_pool = &mut ed_model.module.env.pool;
    let expr2_node = Expr2::EmptyRecord;

    let mark_node_pool = &mut ed_model.markup_node_pool;

    ast_pool.set(ast_node_id, expr2_node);

    let left_bracket_node = MarkupNode::Text {
        content: nodes::LEFT_ACCOLADE.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::new(),
        parent_id_opt: Some(curr_mark_node_id),
    };

    let left_bracket_node_id = mark_node_pool.add(left_bracket_node);

    let right_bracket_node = MarkupNode::Text {
        content: nodes::RIGHT_ACCOLADE.to_owned(),
        ast_node_id,
        syn_high_style: HighlightStyle::Bracket,
        attributes: Attributes::new(),
        parent_id_opt: Some(curr_mark_node_id),
    };

    let right_bracket_node_id = mark_node_pool.add(right_bracket_node);

    let nested_node = MarkupNode::Nested {
        ast_node_id,
        children_ids: vec![left_bracket_node_id, right_bracket_node_id],
        parent_id_opt,
    };

    if is_blank_node {
        mark_node_pool.replace_node(curr_mark_node_id, nested_node);

        for _ in 0..nodes::LEFT_ACCOLADE.len() {
            ed_model.simple_move_carets_right();
        }

        // update GridNodeMap
        ed_model.grid_node_map.add_to_line(
            old_caret_pos.line,
            nodes::LEFT_ACCOLADE.len(),
            left_bracket_node_id,
        )?;

        ed_model.grid_node_map.add_to_line(
            old_caret_pos.line,
            nodes::RIGHT_ACCOLADE.len(),
            right_bracket_node_id,
        )?;
    }

    Ok(())
}

pub fn update_record_field(
    new_input: &str,
    old_caret_pos: TextPos,
    curr_mark_node_id: MarkNodeId,
    new_child_index: usize,
    record_fields: &PoolVec<(PoolStr, Variable, NodeId<Expr2>)>,
    ed_model: &mut EdModel,
) -> EdResult<()> {
    if new_child_index == 2 {
        // update MarkupNode
        let curr_mark_node_mut = ed_model.markup_node_pool.get_mut(curr_mark_node_id);
        let content_str_mut = curr_mark_node_mut.get_content_mut()?;
        content_str_mut.push_str(new_input);

        // update caret
        for _ in 0..new_input.len() {
            ed_model.simple_move_carets_right();
        }

        // update GridNodeMap
        ed_model.grid_node_map.insert_between_line(
            old_caret_pos.line,
            old_caret_pos.column,
            new_input.len(),
            curr_mark_node_id,
        )?;

        // update AST Node
        let first_field = record_fields
            .iter(ed_model.module.env.pool)
            .next()
            .with_context(|| RecordWithoutFields {})?;

        let mut new_field_name = String::new();

        first_field.0.as_str(ed_model.module.env.pool).to_string();

        new_field_name.push_str(new_input);

        let new_pool_str = PoolStr::new(&new_field_name, &mut ed_model.module.env.pool);

        let first_field_mut = record_fields
            .iter_mut(ed_model.module.env.pool)
            .next()
            .with_context(|| RecordWithoutFields {})?;

        first_field_mut.0 = new_pool_str;

        Ok(())
    } else {
        unimplemented!("TODO implement updating of other fields of record.")
    }
}

pub fn update_record_colon(ed_model: &mut EdModel) -> EdResult<()> {
    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    } = get_node_context(&ed_model)?;

    if let Some(parent_id) = parent_id_opt {
        let sibling_ids = curr_mark_node.get_sibling_ids(&ed_model.markup_node_pool);

        let new_child_index = index_of(curr_mark_node_id, &sibling_ids)? + 1;

        let ast_node_ref = ed_model.module.env.pool.get(ast_node_id);

        match ast_node_ref {
            Expr2::Record {
                record_var: _,
                fields,
            } => {
                // update Markup
                let record_colon = nodes::COLON;

                let record_colon_node = MarkupNode::Text {
                    content: record_colon.to_owned(),
                    ast_node_id,
                    syn_high_style: HighlightStyle::Operator,
                    attributes: Attributes::new(),
                    parent_id_opt: Some(parent_id),
                };

                let record_colon_node_id = ed_model.markup_node_pool.add(record_colon_node);
                ed_model
                    .markup_node_pool
                    .get_mut(parent_id)
                    .add_child_at_index(new_child_index, record_colon_node_id)?;

                let record_blank_node = MarkupNode::Blank {
                    ast_node_id,
                    syn_high_style: HighlightStyle::Blank,
                    attributes: Attributes::new(),
                    parent_id_opt: Some(parent_id),
                };

                let record_blank_node_id = ed_model.markup_node_pool.add(record_blank_node);
                ed_model
                    .markup_node_pool
                    .get_mut(parent_id)
                    .add_child_at_index(new_child_index + 1, record_blank_node_id)?;

                // update caret
                for _ in 0..record_colon.len() {
                    ed_model.simple_move_carets_right();
                }

                // update GridNodeMap
                ed_model.grid_node_map.add_to_line(
                    old_caret_pos.line,
                    nodes::COLON.len(),
                    record_colon_node_id,
                )?;

                ed_model.grid_node_map.add_to_line(
                    old_caret_pos.line,
                    nodes::BLANK_PLACEHOLDER.len(),
                    record_blank_node_id,
                )?;

                // update AST node
                let new_field_val = Expr2::Blank;
                let new_field_val_id = ed_model.module.env.pool.add(new_field_val);

                let first_field_mut = fields
                    .iter_mut(ed_model.module.env.pool)
                    .next()
                    .with_context(|| RecordWithoutFields {})?;

                first_field_mut.2 = new_field_val_id;
            }
            other => unimplemented!("TODO implement updating of Expr2 {:?}.", other),
        }

        Ok(())
    } else {
        MissingParent {
            node_id: curr_mark_node_id,
        }
        .fail()
    }
}
