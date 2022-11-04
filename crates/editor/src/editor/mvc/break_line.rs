use roc_ast::lang::core::def::def2::Def2;
use roc_code_markup::markup::common_nodes::NEW_LINES_AFTER_DEF;

use crate::editor::ed_error::EdResult;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::util::index_of;
use crate::ui::text::lines::Lines;
use crate::ui::text::text_pos::TextPos;

// put everything after caret on new line, create a Def2::Blank if there was nothing after the caret.
pub fn break_line(ed_model: &mut EdModel) -> EdResult<InputOutcome> {
    let carets = ed_model.get_carets();

    for caret_pos in carets.iter() {
        let caret_line_nr = caret_pos.line;

        // don't allow adding new lines on empty line
        if caret_pos.column > 0
            && ed_model.grid_node_map.node_exists_at_pos(TextPos {
                line: caret_line_nr,
                column: caret_pos.column - 1,
            })
        {
            let new_blank_line_nr = caret_line_nr + NEW_LINES_AFTER_DEF;
            // if there already is a blank line at new_blank_line_nr just move the caret there, don't add extra lines
            // safe unwrap, we already checked the nr_of_lines
            if !(ed_model.code_lines.nr_of_lines() >= new_blank_line_nr
                && ed_model.code_lines.line_len(new_blank_line_nr).unwrap() == 0)
            {
                for i in 1..=NEW_LINES_AFTER_DEF {
                    EdModel::insert_empty_line(caret_line_nr + i, &mut ed_model.grid_node_map)?;
                }

                insert_new_blank(ed_model, caret_pos.line + NEW_LINES_AFTER_DEF + 1)?;
            }
        }
    }

    ed_model.simple_move_carets_down(NEW_LINES_AFTER_DEF); // one blank lines between top level definitions

    Ok(InputOutcome::Accepted)
}

pub fn insert_new_blank(ed_model: &mut EdModel, insert_on_line_nr: usize) -> EdResult<()> {
    // find position of the previous ASTNode to figure out where to add this new Blank ASTNode
    let def_mark_node_id = ed_model.grid_node_map.get_def_mark_node_id_before_line(
        insert_on_line_nr,
        &ed_model.mark_node_pool,
        &ed_model.mark_id_ast_id_map,
    )?;

    let new_line_blank = Def2::Blank;
    let new_line_blank_id = ed_model.module.env.pool.add(new_line_blank);

    let insertion_index = index_of(def_mark_node_id, &ed_model.markup_ids)?;
    ed_model
        .module
        .ast
        .insert_def_at_index(new_line_blank_id, insertion_index);

    Ok(())
}
