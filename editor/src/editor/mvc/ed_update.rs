use crate::editor::util::index_of;
use crate::editor::grid_node_map::GridNodeMap;
use crate::editor::slow_pool::SlowPool;
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::code_lines::CodeLines;
use crate::editor::ed_error::EdResult;
use crate::editor::markup::attribute::Attributes;
use crate::editor::markup::nodes;
use crate::editor::markup::nodes::MarkupNode;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::syntax_highlight::HighlightStyle;
use crate::lang::ast::Expr2;
use crate::ui::text::caret_w_select::CaretWSelect;
use crate::ui::text::lines::MoveCaretFun;
use crate::ui::text::selection::validate_raw_sel;
use crate::ui::text::selection::RawSelection;
use crate::ui::text::selection::Selection;
use crate::ui::text::text_pos::TextPos;
use crate::ui::text::{lines, lines::Lines, lines::SelectableLines};
use crate::ui::ui_error::UIResult;
use crate::ui::util::is_newline;
use crate::window::keyboard_input::Modifiers;
use winit::event::VirtualKeyCode;
use VirtualKeyCode::*;

impl<'a> EdModel<'a> {
    pub fn move_caret(
        &mut self,
        move_fun: MoveCaretFun<CodeLines>,
        modifiers: &Modifiers,
    ) -> UIResult<()> {
        for caret_tup in self.caret_w_select_vec.iter_mut() {
            caret_tup.0 = move_fun(&self.code_lines, caret_tup.0, modifiers)?;
            caret_tup.1 = None;
        }

        Ok(())
    }

    // TODO delete
    // disregards EdModel.code_lines because the caller knows the resulting caret position will be valid.
    // allows us to prevent multiple updates to EdModel.code_lines
    pub fn simple_move_carets_right(&mut self) {
        for caret_tup in self.caret_w_select_vec.iter_mut() {
            caret_tup.0.caret_pos.column += 1;
            caret_tup.1 = None;
        }
    }

    pub fn build_node_map_from_markup(markup_root_id: MarkNodeId, markup_node_pool: &SlowPool) -> EdResult<GridNodeMap> {
        let mut grid_node_map = GridNodeMap::new();

        EdModel::build_grid_node_map(markup_root_id, &mut grid_node_map, markup_node_pool);

        Ok(grid_node_map)
    }

    fn build_grid_node_map(node_id: MarkNodeId, grid_node_map: &mut GridNodeMap, markup_node_pool: &SlowPool)  -> EdResult<()> {
        let node = markup_node_pool.get(node_id);

        if node.is_nested() {
            for child_id in node.get_children_ids() {
                EdModel::build_grid_node_map(child_id, grid_node_map, markup_node_pool)?;
            }
        } else {
            let node_content_str = node.get_content()?;

            grid_node_map.add_to_line(0, node_content_str.len(), node_id);
        }

        Ok(())
    }

    pub fn build_code_lines_from_markup(markup_root_id: MarkNodeId, markup_node_pool: &SlowPool) -> EdResult<CodeLines> {
        let mut all_code_string = String::new();

        EdModel::build_markup_string(markup_root_id, &mut all_code_string, markup_node_pool)?;
    
        let code_lines = CodeLines::from_str(&all_code_string);

        Ok(code_lines)
    }

    fn build_markup_string(node_id: MarkNodeId, all_code_string: &mut String, markup_node_pool: &SlowPool) -> EdResult<()> {
        let node = markup_node_pool.get(node_id);

        if node.is_nested() {
            for child_id in node.get_children_ids() {
                EdModel::build_markup_string(child_id, all_code_string, markup_node_pool)?;
            }
        } else {
            let node_content_str = node.get_content()?;

            all_code_string.push_str(&node_content_str);
        }

        Ok(())
    }
}

impl<'a> SelectableLines for EdModel<'a> {
    fn get_caret(self) -> TextPos {
        self.caret_w_select_vec.first().0.caret_pos
    }

    // keeps active selection
    fn set_caret(&mut self, caret_pos: TextPos) {
        let caret_tup = self.caret_w_select_vec.first_mut();
        caret_tup.0.caret_pos = caret_pos;
        caret_tup.1 = None;
    }

    fn move_caret_left(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let move_fun: MoveCaretFun<CodeLines> = lines::move_caret_left;
        EdModel::move_caret(self, move_fun, modifiers)?;

        Ok(())
    }

    fn move_caret_right(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let move_fun: MoveCaretFun<CodeLines> = lines::move_caret_right;
        EdModel::move_caret(self, move_fun, modifiers)?;

        Ok(())
    }

    fn move_caret_up(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let move_fun: MoveCaretFun<CodeLines> = lines::move_caret_up;
        EdModel::move_caret(self, move_fun, modifiers)?;

        Ok(())
    }

    fn move_caret_down(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let move_fun: MoveCaretFun<CodeLines> = lines::move_caret_down;
        EdModel::move_caret(self, move_fun, modifiers)?;

        Ok(())
    }

    fn move_caret_home(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let move_fun: MoveCaretFun<CodeLines> = lines::move_caret_home;
        EdModel::move_caret(self, move_fun, modifiers)?;

        Ok(())
    }

    fn move_caret_end(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let move_fun: MoveCaretFun<CodeLines> = lines::move_caret_end;
        EdModel::move_caret(self, move_fun, modifiers)?;

        Ok(())
    }

    fn get_selection(&self) -> Option<Selection> {
        self.caret_w_select_vec.first().0.selection_opt
    }

    fn is_selection_active(&self) -> bool {
        self.get_selection().is_some()
    }

    fn get_selected_str(&self) -> UIResult<Option<String>> {
        if let Some(selection) = self.get_selection() {
            let start_line_index = selection.start_pos.line;
            let start_col = selection.start_pos.column;
            let end_line_index = selection.end_pos.line;
            let end_col = selection.end_pos.column;

            if start_line_index == end_line_index {
                let line_ref = self.code_lines.get_line(start_line_index)?;

                Ok(Some(line_ref[start_col..end_col].to_string()))
            } else {
                let full_str = String::new();

                // TODO
                Ok(Some(full_str))
            }
        } else {
            Ok(None)
        }
    }

    fn set_raw_sel(&mut self, raw_sel: RawSelection) -> UIResult<()> {
        self.caret_w_select_vec.first_mut().0.selection_opt = Some(validate_raw_sel(raw_sel)?);

        Ok(())
    }

    fn set_sel_none(&mut self) {
        self.caret_w_select_vec.first_mut().0.selection_opt = None;
    }

    fn set_caret_w_sel(&mut self, caret_w_sel: CaretWSelect) {
        self.caret_w_select_vec.first_mut().0 = caret_w_sel;
    }

    fn select_all(&mut self) -> UIResult<()> {
        if self.code_lines.nr_of_chars() > 0 {
            let last_pos = self.last_text_pos()?;

            self.set_raw_sel(RawSelection {
                start_pos: TextPos { line: 0, column: 0 },
                end_pos: last_pos,
            })?;

            self.set_caret(last_pos);
        }

        Ok(())
    }

    fn last_text_pos(&self) -> UIResult<TextPos> {
        let nr_of_lines = self.code_lines.lines.len();
        let last_line_index = nr_of_lines - 1;
        let last_line = self.code_lines.get_line(last_line_index)?;

        Ok(TextPos {
            line: self.code_lines.lines.len() - 1,
            column: last_line.len(),
        })
    }

    fn handle_key_down(
        &mut self,
        modifiers: &Modifiers,
        virtual_keycode: VirtualKeyCode,
    ) -> UIResult<()> {
        match virtual_keycode {
            Left => self.move_caret_left(modifiers),
            Up => self.move_caret_up(modifiers),
            Right => self.move_caret_right(modifiers),
            Down => self.move_caret_down(modifiers),

            A => {
                if modifiers.ctrl {
                    self.select_all()
                } else {
                    Ok(())
                }
            }
            Home => self.move_caret_home(modifiers),
            End => self.move_caret_end(modifiers),
            _ => Ok(()),
        }
    }
}

pub fn handle_new_char(received_char: &char, ed_model: &mut EdModel) -> EdResult<()> {
    // TODO set all selections to none

    match received_char {
        '{' => {
            let old_caret_pos = ed_model.get_caret();

            let curr_mark_node_id = ed_model.grid_node_map.get_id_at_row_col(old_caret_pos)?;
            let curr_mark_node = ed_model.markup_node_pool.get(curr_mark_node_id);
            let is_blank_node = curr_mark_node.is_blank();
            let parent_id_opt = curr_mark_node.get_parent_id_opt();
            let ast_node_id = curr_mark_node.get_ast_node_id();

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
            }
        }
        '\u{8}' | '\u{7f}' => {
            // On Linux, '\u{8}' is backspace,
            // On macOS '\u{7f}'.
            unimplemented!("TODO implement backspace")
        }
        ch if is_newline(ch) => {
            unimplemented!("TODO implement newline")
        }
        '\u{1}' // Ctrl + A
        | '\u{3}' // Ctrl + C
        | '\u{16}' // Ctrl + V
        | '\u{18}' // Ctrl + X
        | '\u{e000}'..='\u{f8ff}' // http://www.unicode.org/faq/private_use.html
        | '\u{f0000}'..='\u{ffffd}' // ^
        | '\u{100000}'..='\u{10fffd}' // ^
        => {
            // chars that can be ignored
        }
        ch => {
            let old_caret_pos = ed_model.get_caret();

            let curr_mark_node_id = ed_model.grid_node_map.get_id_at_row_col(old_caret_pos)?;
            let curr_mark_node = ed_model.markup_node_pool.get(curr_mark_node_id);
            let parent_id_opt = curr_mark_node.get_parent_id_opt();
            let ast_node_id = curr_mark_node.get_ast_node_id();
            
            if let Some(parent_id) = parent_id_opt {
                let sibling_ids = curr_mark_node.get_sibling_ids(&ed_model.markup_node_pool);

                let new_child_index = index_of(curr_mark_node_id, &sibling_ids);

                // TODO match, create new MarkupNode + ASTNode
            }
        }
    }

    Ok(())
}

/*
    let old_caret_pos = ed_model.caret_pos;

    match received_char {
        '\u{8}' | '\u{7f}' => {
            // On Linux, '\u{8}' is backspace,
            // on macOS '\u{7f}'.
            if let Some(selection) = ed_model.selection_opt {
                del_selection(selection, ed_model)?;
            } else {
                ed_model.caret_pos =
                    move_caret_left(old_caret_pos, None, false, &ed_model.text_buf).0;

                ed_model.text_buf.pop_char(old_caret_pos);
            }

            ed_model.selection_opt = None;
        }
        ch if is_newline(ch) => {
            if let Some(selection) = ed_model.selection_opt {
                del_selection(selection, ed_model)?;
                ed_model.text_buf.insert_char(ed_model.caret_pos, &'\n')?;
            } else {
                ed_model.text_buf.insert_char(old_caret_pos, &'\n')?;

                ed_model.caret_pos = Position {
                    line: old_caret_pos.line + 1,
                    column: 0,
                };
            }

            ed_model.selection_opt = None;
        }
        '\u{1}' // Ctrl + A
        | '\u{3}' // Ctrl + C
        | '\u{16}' // Ctrl + V
        | '\u{18}' // Ctrl + X
        | '\u{e000}'..='\u{f8ff}' // http://www.unicode.org/faq/private_use.html
        | '\u{f0000}'..='\u{ffffd}' // ^
        | '\u{100000}'..='\u{10fffd}' // ^
        => {
            // chars that can be ignored
        }
        _ => {
            if let Some(selection) = ed_model.selection_opt {
                del_selection(selection, ed_model)?;
                ed_model
                    .text_buf
                    .insert_char(ed_model.caret_pos, received_char)?;

                ed_model.caret_pos =
                    move_caret_right(ed_model.caret_pos, None, false, &ed_model.text_buf).0;
            } else {
                ed_model
                    .text_buf
                    .insert_char(old_caret_pos, received_char)?;

                ed_model.caret_pos = Position {
                    line: old_caret_pos.line,
                    column: old_caret_pos.column + 1,
                };
            }

            ed_model.selection_opt = None;
        }
    }

    Ok(())
*/
