use crate::editor::code_lines::CodeLines;
use crate::editor::ed_error::EdResult;
use crate::editor::grid_node_map::GridNodeMap;
use crate::editor::markup::attribute::Attributes;
use crate::editor::markup::nodes;
use crate::editor::markup::nodes::MarkupNode;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::record_update::start_new_record;
use crate::editor::mvc::record_update::update_record_colon;
use crate::editor::mvc::record_update::update_record_field;
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::slow_pool::SlowPool;
use crate::editor::syntax_highlight::HighlightStyle;
use crate::editor::util::index_of;
use crate::lang::ast::Expr2;
use crate::lang::pool::{NodeId, PoolStr, PoolVec};
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

    pub fn build_node_map_from_markup(
        markup_root_id: MarkNodeId,
        markup_node_pool: &SlowPool,
    ) -> EdResult<GridNodeMap> {
        let mut grid_node_map = GridNodeMap::new();

        EdModel::build_grid_node_map(markup_root_id, &mut grid_node_map, markup_node_pool)?;

        Ok(grid_node_map)
    }

    fn build_grid_node_map(
        node_id: MarkNodeId,
        grid_node_map: &mut GridNodeMap,
        markup_node_pool: &SlowPool,
    ) -> EdResult<()> {
        let node = markup_node_pool.get(node_id);

        if node.is_nested() {
            for child_id in node.get_children_ids() {
                EdModel::build_grid_node_map(child_id, grid_node_map, markup_node_pool)?;
            }
        } else {
            let node_content_str = node.get_content()?;

            grid_node_map.add_to_line(0, node_content_str.len(), node_id)?;
        }

        Ok(())
    }

    pub fn build_code_lines_from_markup(
        markup_root_id: MarkNodeId,
        markup_node_pool: &SlowPool,
    ) -> EdResult<CodeLines> {
        let mut all_code_string = String::new();

        EdModel::build_markup_string(markup_root_id, &mut all_code_string, markup_node_pool)?;

        let code_lines = CodeLines::from_str(&all_code_string);

        Ok(code_lines)
    }

    fn build_markup_string(
        node_id: MarkNodeId,
        all_code_string: &mut String,
        markup_node_pool: &SlowPool,
    ) -> EdResult<()> {
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
    fn get_caret(&self) -> TextPos {
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

pub struct NodeContext<'a> {
    pub old_caret_pos: TextPos,
    pub curr_mark_node_id: MarkNodeId,
    pub curr_mark_node: &'a MarkupNode,
    pub parent_id_opt: Option<MarkNodeId>,
    pub ast_node_id: NodeId<Expr2>,
}

pub fn get_node_context<'a>(ed_model: &'a EdModel) -> EdResult<NodeContext<'a>> {
    let old_caret_pos = ed_model.get_caret();
    let curr_mark_node_id = ed_model
        .grid_node_map
        .get_id_at_row_col(ed_model.get_caret())?;
    let curr_mark_node = ed_model.markup_node_pool.get(curr_mark_node_id);
    let parent_id_opt = curr_mark_node.get_parent_id_opt();
    let ast_node_id = curr_mark_node.get_ast_node_id();

    Ok(NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    })
}

pub fn handle_new_char(received_char: &char, ed_model: &mut EdModel) -> EdResult<()> {
    // TODO set all selections to none
    // TODO nested records

    match received_char {
        '{' => {
            start_new_record(ed_model)?;
        }
        ':' => {
            // TODO set up Dict if previous char is '{'
            update_record_colon(ed_model)?;
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
            let curr_mark_node_id = ed_model.grid_node_map.get_id_at_row_col(ed_model.get_caret())?;
            let curr_mark_node = ed_model.markup_node_pool.get(curr_mark_node_id);
            let parent_id_opt = curr_mark_node.get_parent_id_opt();
            let ast_node_id = curr_mark_node.get_ast_node_id();

            let ast_node_ref = ed_model.module.env.pool.get(ast_node_id);

            if let Some(parent_id) = parent_id_opt {
                let sibling_ids = curr_mark_node.get_sibling_ids(&ed_model.markup_node_pool);

                let new_child_index = index_of(curr_mark_node_id, &sibling_ids)? + 1;

                match ast_node_ref {
                    Expr2::EmptyRecord => {
                        if curr_mark_node.get_content()? == nodes::LEFT_ACCOLADE {

                            // update Markup
                            let record_field_str = &ch.to_string();

                            let record_field_node = MarkupNode::Text {
                                content: record_field_str.to_owned(),
                                ast_node_id,
                                syn_high_style: HighlightStyle::RecordField,
                                attributes: Attributes::new(),
                                parent_id_opt: Some(parent_id),
                            };

                            let record_field_node_id = ed_model.markup_node_pool.add(record_field_node);

                            let parent = ed_model.markup_node_pool.get_mut(parent_id);
                            parent.add_child_at_index(new_child_index, record_field_node_id)?;

                            // update caret
                            ed_model.simple_move_carets_right();

                            // update GridNodeMap
                            ed_model.grid_node_map.insert_between_line(
                                old_caret_pos.line,
                                old_caret_pos.column + 1,
                                record_field_str.len(),
                                record_field_node_id,
                            )?;

                            // update AST
                            let record_var = ed_model.module.env.var_store.fresh();
                            let field_name = PoolStr::new(record_field_str, &mut ed_model.module.env.pool);
                            let field_var = ed_model.module.env.var_store.fresh();
                            //TODO actually check if field_str belongs to a previously defined variable
                            let field_val = Expr2::InvalidLookup(
                                                PoolStr::new(record_field_str, ed_model.module.env.pool)
                                            );
                            let field_val_id = ed_model.module.env.pool.add(field_val);
                            let first_field = (field_name, field_var, field_val_id);

                            let fields = PoolVec::new(
                                vec![first_field].into_iter(),
                                &mut ed_model.module.env.pool,
                            );

                            let new_ast_node =
                                Expr2::Record {
                                    record_var,
                                    fields,
                                };

                            ed_model.module.env.pool.set(ast_node_id, new_ast_node);
                        } else {
                            unimplemented!("TODO handle this else")
                        }
                    },
                    Expr2::Record { record_var:_, fields } => {
                        update_record_field(
                            &ch.to_string(),
                            old_caret_pos,
                            curr_mark_node_id,
                            new_child_index,
                            fields,
                            ed_model,
                        )?;
                    },
                    other => {
                        unimplemented!("TODO implement updating of Expr2 {:?}.", other)
                    },
                }
            }
        }
    }

    Ok(())
}
