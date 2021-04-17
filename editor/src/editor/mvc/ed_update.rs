use crate::editor::code_lines::CodeLines;
use crate::editor::ed_error::from_ui_res;
use crate::editor::ed_error::print_ui_err;
use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::MissingSelection;
use crate::editor::grid_node_map::GridNodeMap;
use crate::editor::markup::attribute::Attributes;
use crate::editor::markup::nodes;
use crate::editor::markup::nodes::MarkupNode;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::lookup_update::update_invalid_lookup;
use crate::editor::mvc::record_update::start_new_record;
use crate::editor::mvc::record_update::update_empty_record;
use crate::editor::mvc::record_update::update_record_colon;
use crate::editor::mvc::record_update::update_record_field;
use crate::editor::mvc::string_update::start_new_string;
use crate::editor::mvc::string_update::update_small_string;
use crate::editor::mvc::string_update::update_string;
use crate::editor::slow_pool::MarkNodeId;
use crate::editor::slow_pool::SlowPool;
use crate::editor::syntax_highlight::HighlightStyle;
use crate::lang::ast::Expr2;
use crate::lang::pool::NodeId;
use crate::ui::text::caret_w_select::CaretWSelect;
use crate::ui::text::lines::MoveCaretFun;
use crate::ui::text::selection::validate_raw_sel;
use crate::ui::text::selection::RawSelection;
use crate::ui::text::selection::Selection;
use crate::ui::text::text_pos::TextPos;
use crate::ui::text::{lines, lines::Lines, lines::SelectableLines};
use crate::ui::ui_error::UIResult;
use crate::window::keyboard_input::Modifiers;
use snafu::OptionExt;
use winit::event::VirtualKeyCode;
use VirtualKeyCode::*;

impl<'a> EdModel<'a> {
    pub fn move_caret(
        &mut self,
        move_fun: MoveCaretFun<CodeLines>,
        modifiers: &Modifiers,
    ) -> UIResult<()> {
        self.dirty = true;

        for caret_tup in self.caret_w_select_vec.iter_mut() {
            caret_tup.0 = move_fun(&self.code_lines, caret_tup.0, modifiers)?;
            caret_tup.1 = None;
        }
        self.selected_expr2_tup = None;

        Ok(())
    }

    // disregards EdModel.code_lines because the caller knows the resulting caret position will be valid.
    // allows us to prevent multiple updates to EdModel.code_lines
    pub fn simple_move_carets_right(&mut self, repeat: usize) {
        for caret_tup in self.caret_w_select_vec.iter_mut() {
            caret_tup.0.caret_pos.column += repeat;
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

    // updates grid_node_map and code_lines but nothing else.
    pub fn insert_between_line(
        &mut self,
        line_nr: usize,
        index: usize,
        new_str: &str,
        node_id: MarkNodeId,
    ) -> UIResult<()> {
        self.grid_node_map
            .insert_between_line(line_nr, index, new_str.len(), node_id)?;
        self.code_lines.insert_between_line(line_nr, index, new_str)
    }

    // updates grid_node_map and code_lines but nothing else.
    pub fn del_at_line(&mut self, line_nr: usize, index: usize) -> UIResult<()> {
        self.grid_node_map.del_at_line(line_nr, index)?;
        self.code_lines.del_at_line(line_nr, index)
    }

    // select all MarkupNodes that refer to specific ast node and its children.
    pub fn select_expr(&mut self) -> EdResult<()> {
        // include parent in selection if an `Expr2` was already selected
        if let Some((_sel_expr2_id, mark_node_id)) = self.selected_expr2_tup {
            let expr2_level_mark_node = self.markup_node_pool.get(mark_node_id);

            if let Some(parent_id) = expr2_level_mark_node.get_parent_id_opt() {
                let parent_mark_node = self.markup_node_pool.get(parent_id);
                let ast_node_id = parent_mark_node.get_ast_node_id();

                let (expr_start_pos, expr_end_pos) = self
                    .grid_node_map
                    .get_nested_start_end_pos(parent_id, self)?;

                self.set_raw_sel(RawSelection {
                    start_pos: expr_start_pos,
                    end_pos: expr_end_pos,
                })?;

                self.set_caret(expr_start_pos);
                self.selected_expr2_tup = Some((ast_node_id, parent_id));

                self.dirty = true;
            }
        } else {
            // select `Expr2` in which caret is currently positioned
            let caret_pos = self.get_caret();
            if self.grid_node_map.node_exists_at_pos(caret_pos) {
                let (expr_start_pos, expr_end_pos, ast_node_id, mark_node_id) = self
                    .grid_node_map
                    .get_expr_start_end_pos(self.get_caret(), &self)?;
                self.set_raw_sel(RawSelection {
                    start_pos: expr_start_pos,
                    end_pos: expr_end_pos,
                })?;

                self.set_caret(expr_start_pos);
                self.selected_expr2_tup = Some((ast_node_id, mark_node_id));

                self.dirty = true;
            }
        }

        Ok(())
    }

    pub fn ed_handle_key_down(
        &mut self,
        modifiers: &Modifiers,
        virtual_keycode: VirtualKeyCode,
    ) -> EdResult<()> {
        match virtual_keycode {
            Left => from_ui_res(self.move_caret_left(modifiers)),
            Up => {
                if modifiers.ctrl && modifiers.shift {
                    self.select_expr()
                } else {
                    from_ui_res(self.move_caret_up(modifiers))
                }
            }
            Right => from_ui_res(self.move_caret_right(modifiers)),
            Down => from_ui_res(self.move_caret_down(modifiers)),

            A => {
                if modifiers.ctrl {
                    from_ui_res(self.select_all())
                } else {
                    Ok(())
                }
            }
            Home => from_ui_res(self.move_caret_home(modifiers)),
            End => from_ui_res(self.move_caret_end(modifiers)),
            F11 => {
                self.show_debug_view = !self.show_debug_view;
                self.dirty = true;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn replace_slected_expr_with_blank(&mut self) -> EdResult<()> {
        if let Some((sel_expr2_id, mark_node_id)) = self.selected_expr2_tup {
            let expr2_level_mark_node = self.markup_node_pool.get(mark_node_id);

            let blank_replacement = MarkupNode::Blank {
                ast_node_id: sel_expr2_id,
                attributes: Attributes::new(),
                syn_high_style: HighlightStyle::Blank,
                parent_id_opt: expr2_level_mark_node.get_parent_id_opt(),
            };

            self.markup_node_pool
                .replace_node(mark_node_id, blank_replacement);

            let active_selection = self.get_selection().context(MissingSelection {})?;
            self.code_lines.del_selection(active_selection)?;
            self.grid_node_map.del_selection(active_selection)?;

            let caret_pos = self.get_caret();
            self.insert_between_line(
                caret_pos.line,
                caret_pos.column,
                nodes::BLANK_PLACEHOLDER,
                mark_node_id,
            )?;

            self.module.env.pool.set(sel_expr2_id, Expr2::Blank)
        }

        self.set_sel_none();

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
        self.selected_expr2_tup = None;
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
        _modifiers: &Modifiers,
        _virtual_keycode: VirtualKeyCode,
    ) -> UIResult<()> {
        unreachable!("Use EdModel::ed_handle_key_down instead.")
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

pub fn handle_new_char(received_char: &char, ed_model: &mut EdModel) -> EdResult<InputOutcome> {
    let input_outcome = match received_char {
            '\u{1}' // Ctrl + A
            | '\u{3}' // Ctrl + C
            | '\u{16}' // Ctrl + V
            | '\u{18}' // Ctrl + X
            | '\u{e000}'..='\u{f8ff}' // http://www.unicode.org/faq/private_use.html
            | '\u{f0000}'..='\u{ffffd}' // ^
            | '\u{100000}'..='\u{10fffd}' // ^
            => {
                // chars that can be ignored
                InputOutcome::Ignored
            }
            '\u{8}' | '\u{7f}' => {
                // On Linux, '\u{8}' is backspace,
                // on macOS '\u{7f}'.

                ed_model.replace_slected_expr_with_blank()?;

                InputOutcome::Accepted
            }
            ch => {
                let curr_mark_node_id_res = ed_model.get_curr_mark_node_id();

                let outcome =
                    match curr_mark_node_id_res {
                        Ok(curr_mark_node_id) => {
                            let curr_mark_node = ed_model.markup_node_pool.get(curr_mark_node_id);
                            let prev_mark_node_id_opt = ed_model.get_prev_mark_node_id()?;

                            let ast_node_id = curr_mark_node.get_ast_node_id();
                            let ast_node_ref = ed_model.module.env.pool.get(ast_node_id);

                            if let Expr2::Blank {..} = ast_node_ref {
                                match ch {
                                    '"' => {
                                        start_new_string(ed_model)?
                                    },
                                    '{' => {
                                        start_new_record(ed_model)?
                                    }
                                    _ => InputOutcome::Ignored
                                }
                            } else if let Some(prev_mark_node_id) = prev_mark_node_id_opt{
                                if prev_mark_node_id == curr_mark_node_id {
                                    match ast_node_ref {
                                        Expr2::SmallStr(old_arr_str) => {
                                            update_small_string(
                                                &ch, old_arr_str, ed_model
                                            )?
                                        }
                                        Expr2::Str(old_pool_str) => {
                                            update_string(
                                                &ch.to_string(), old_pool_str, ed_model
                                            )?
                                        }
                                        Expr2::InvalidLookup(old_pool_str) => {
                                            update_invalid_lookup(
                                                &ch.to_string(),
                                                old_pool_str,
                                                curr_mark_node_id,
                                                ast_node_id,
                                                ed_model
                                            )?
                                        }
                                        Expr2::EmptyRecord => {
                                            // prev_mark_node_id and curr_mark_node_id should be different to allow creating field at current caret position
                                            InputOutcome::Ignored
                                        }
                                        Expr2::Record{ record_var:_, fields } => {
                                            if curr_mark_node.get_content()?.chars().all(|chr| chr.is_ascii_alphanumeric()){
                                                update_record_field(
                                                    &ch.to_string(),
                                                    ed_model.get_caret(),
                                                    curr_mark_node_id,
                                                    fields,
                                                    ed_model,
                                                )?
                                            } else {
                                                InputOutcome::Ignored
                                            }
                                        }
                                        _ => InputOutcome::Ignored
                                    }
                                } else if ch.is_ascii_alphanumeric() { // prev_mark_node_id != curr_mark_node_id
                                    let prev_ast_node_id =
                                        ed_model
                                        .markup_node_pool
                                        .get(prev_mark_node_id)
                                        .get_ast_node_id();

                                    let prev_node_ref = ed_model.module.env.pool.get(prev_ast_node_id);

                                    match prev_node_ref {
                                        Expr2::InvalidLookup(old_pool_str) => {
                                            update_invalid_lookup(
                                                &ch.to_string(),
                                                old_pool_str,
                                                prev_mark_node_id,
                                                prev_ast_node_id,
                                                ed_model
                                            )?
                                        }
                                        Expr2::Record{ record_var:_, fields } => {
                                            let prev_mark_node = ed_model.markup_node_pool.get(prev_mark_node_id);

                                            if (curr_mark_node.get_content()? == nodes::RIGHT_ACCOLADE || curr_mark_node.get_content()? == nodes::COLON) &&
                                                prev_mark_node.is_all_alphanumeric()? {
                                                update_record_field(
                                                    &ch.to_string(),
                                                    ed_model.get_caret(),
                                                    prev_mark_node_id,
                                                    fields,
                                                    ed_model,
                                                )?
                                            } else if prev_mark_node.get_content()? == nodes::LEFT_ACCOLADE && curr_mark_node.is_all_alphanumeric()? {
                                                update_record_field(
                                                    &ch.to_string(),
                                                    ed_model.get_caret(),
                                                    curr_mark_node_id,
                                                    fields,
                                                    ed_model,
                                                )?
                                            } else {
                                                InputOutcome::Ignored
                                            }
                                        }
                                        _ => {
                                            match ast_node_ref {
                                                Expr2::EmptyRecord => {
                                                    let sibling_ids = curr_mark_node.get_sibling_ids(&ed_model.markup_node_pool);

                                                    if ch.is_ascii_alphabetic() && ch.is_ascii_lowercase() {
                                                        update_empty_record(
                                                            &ch.to_string(),
                                                            prev_mark_node_id,
                                                            sibling_ids,
                                                            ed_model
                                                        )?
                                                    } else {
                                                        InputOutcome::Ignored
                                                    }
                                                }
                                                _ => InputOutcome::Ignored
                                            }
                                        }
                                    }
                                } else if *ch == ':' {
                                    let mark_parent_id_opt = curr_mark_node.get_parent_id_opt();

                                    if let Some(mark_parent_id) = mark_parent_id_opt {
                                        let parent_ast_id = ed_model.markup_node_pool.get(mark_parent_id).get_ast_node_id();

                                        update_record_colon(ed_model, parent_ast_id)?
                                    } else {
                                        InputOutcome::Ignored
                                    }
                                } else {
                                    InputOutcome::Ignored
                                }

                            } else {
                                // Not supporting any Expr2 right now that would allow prepending at the start of a line
                                InputOutcome::Ignored
                            }
                        },
                        Err(e) => {
                            print_ui_err(&e);
                            InputOutcome::Ignored
                        }
                    };

                    if let InputOutcome::Accepted = outcome {
                        ed_model.set_sel_none();
                    }

                    outcome
            }
        };

    if let InputOutcome::Accepted = input_outcome {
        ed_model.dirty = true;
    }

    Ok(input_outcome)
}

#[cfg(test)]
pub mod test_ed_update {
    use crate::editor::mvc::ed_model::test_ed_model::ed_model_from_dsl;
    use crate::editor::mvc::ed_model::test_ed_model::ed_model_to_dsl;
    use crate::editor::mvc::ed_model::test_ed_model::init_model_refs;
    use crate::editor::mvc::ed_update::handle_new_char;
    use crate::editor::mvc::ed_update::EdResult;
    use crate::ui::ui_error::UIResult;
    use bumpalo::collections::String as BumpString;
    use bumpalo::Bump;

    fn ed_res_to_res<T>(ed_res: EdResult<T>) -> Result<T, String> {
        match ed_res {
            Ok(t) => Ok(t),
            Err(e) => Err(e.to_string()),
        }
    }

    fn ui_res_to_res<T>(ed_res: UIResult<T>) -> Result<T, String> {
        match ed_res {
            Ok(t) => Ok(t),
            Err(e) => Err(e.to_string()),
        }
    }

    pub fn assert_insert(
        pre_lines: &[&str],
        expected_post_lines: &[&str],
        new_char: char,
    ) -> Result<(), String> {
        assert_insert_seq(pre_lines, expected_post_lines, &new_char.to_string())
    }

    pub fn assert_insert_seq(
        pre_lines: &[&str],
        expected_post_lines: &[&str],
        new_char_seq: &str,
    ) -> Result<(), String> {
        let test_arena = Bump::new();
        let code_str = BumpString::from_str_in(&pre_lines.join("").replace("┃", ""), &test_arena);

        let mut model_refs = init_model_refs();

        let mut ed_model = ed_model_from_dsl(&code_str, pre_lines, &mut model_refs)?;

        for input_char in new_char_seq.chars() {
            ed_res_to_res(handle_new_char(&input_char, &mut ed_model))?;
        }

        let post_lines = ui_res_to_res(ed_model_to_dsl(&ed_model))?;

        assert_eq!(post_lines, expected_post_lines);

        Ok(())
    }

    pub fn assert_insert_seq_ignore(lines: &[&str], new_char_seq: &str) -> Result<(), String> {
        assert_insert_seq(lines, lines, new_char_seq)
    }

    #[test]
    fn test_ignore_basic() -> Result<(), String> {
        // space is added because Blank is inserted
        assert_insert(&["┃"], &["┃ "], 'a')?;
        assert_insert(&["┃"], &["┃ "], ';')?;
        assert_insert(&["┃"], &["┃ "], '5')?;
        assert_insert(&["┃"], &["┃ "], '-')?;
        assert_insert(&["┃"], &["┃ "], '_')?;

        Ok(())
    }

    #[test]
    fn test_string() -> Result<(), String> {
        assert_insert(&["┃"], &["\"┃\""], '"')?;
        assert_insert(&["\"┃\""], &["\"a┃\""], 'a')?;
        assert_insert(&["\"┃\""], &["\"{┃\""], '{')?;
        assert_insert(&["\"┃\""], &["\"}┃\""], '}')?;
        assert_insert(&["\"┃\""], &["\"[┃\""], '[')?;
        assert_insert(&["\"┃\""], &["\"]┃\""], ']')?;
        assert_insert(&["\"┃\""], &["\"-┃\""], '-')?;
        assert_insert(&["\"┃-\""], &["\"<┃-\""], '<')?;
        assert_insert(&["\"-┃\""], &["\"->┃\""], '>')?;

        assert_insert(&["\"a┃\""], &["\"ab┃\""], 'b')?;
        assert_insert(&["\"ab┃\""], &["\"abc┃\""], 'c')?;
        assert_insert(&["\"┃a\""], &["\"z┃a\""], 'z')?;
        assert_insert(&["\"┃a\""], &["\" ┃a\""], ' ')?;
        assert_insert(&["\"a┃b\""], &["\"az┃b\""], 'z')?;
        assert_insert(&["\"a┃b\""], &["\"a ┃b\""], ' ')?;

        assert_insert(&["\"ab ┃\""], &["\"ab {┃\""], '{')?;
        assert_insert(&["\"ab ┃\""], &["\"ab }┃\""], '}')?;
        assert_insert(&["\"{ str: 4┃}\""], &["\"{ str: 44┃}\""], '4')?;
        assert_insert(
            &["\"┃ello, hello, hello\""],
            &["\"h┃ello, hello, hello\""],
            'h',
        )?;
        assert_insert(
            &["\"hello┃ hello, hello\""],
            &["\"hello,┃ hello, hello\""],
            ',',
        )?;
        assert_insert(
            &["\"hello, hello, hello┃\""],
            &["\"hello, hello, hello.┃\""],
            '.',
        )?;

        Ok(())
    }

    #[test]
    fn test_ignore_string() -> Result<(), String> {
        assert_insert(&["┃\"\""], &["┃\"\""], 'a')?;
        assert_insert(&["┃\"\""], &["┃\"\""], 'A')?;
        assert_insert(&["┃\"\""], &["┃\"\""], '"')?;
        assert_insert(&["┃\"\""], &["┃\"\""], '{')?;
        assert_insert(&["┃\"\""], &["┃\"\""], '[')?;
        assert_insert(&["┃\"\""], &["┃\"\""], '}')?;
        assert_insert(&["┃\"\""], &["┃\"\""], ']')?;
        assert_insert(&["┃\"\""], &["┃\"\""], '-')?;

        assert_insert(&["\"\"┃"], &["\"\"┃"], 'a')?;
        assert_insert(&["\"\"┃"], &["\"\"┃"], 'A')?;
        assert_insert(&["\"\"┃"], &["\"\"┃"], '"')?;
        assert_insert(&["\"\"┃"], &["\"\"┃"], '{')?;
        assert_insert(&["\"\"┃"], &["\"\"┃"], '[')?;
        assert_insert(&["\"\"┃"], &["\"\"┃"], '}')?;
        assert_insert(&["\"\"┃"], &["\"\"┃"], ']')?;
        assert_insert(&["\"\"┃"], &["\"\"┃"], '-')?;

        assert_insert(&["┃\"a\""], &["┃\"a\""], 'a')?;
        assert_insert(&["┃\"a\""], &["┃\"a\""], 'A')?;
        assert_insert(&["┃\"a\""], &["┃\"a\""], '"')?;
        assert_insert(&["┃\"a\""], &["┃\"a\""], '{')?;
        assert_insert(&["┃\"a\""], &["┃\"a\""], '[')?;
        assert_insert(&["┃\"a\""], &["┃\"a\""], '}')?;
        assert_insert(&["┃\"a\""], &["┃\"a\""], ']')?;
        assert_insert(&["┃\"a\""], &["┃\"a\""], '-')?;

        assert_insert(&["\"a\"┃"], &["\"a\"┃"], 'a')?;
        assert_insert(&["\"a\"┃"], &["\"a\"┃"], 'A')?;
        assert_insert(&["\"a\"┃"], &["\"a\"┃"], '"')?;
        assert_insert(&["\"a\"┃"], &["\"a\"┃"], '{')?;
        assert_insert(&["\"a\"┃"], &["\"a\"┃"], '[')?;
        assert_insert(&["\"a\"┃"], &["\"a\"┃"], '}')?;
        assert_insert(&["\"a\"┃"], &["\"a\"┃"], ']')?;
        assert_insert(&["\"a\"┃"], &["\"a\"┃"], '-')?;

        assert_insert(&["┃\"{  }\""], &["┃\"{  }\""], 'a')?;
        assert_insert(&["┃\"{  }\""], &["┃\"{  }\""], 'A')?;
        assert_insert(&["┃\"{  }\""], &["┃\"{  }\""], '"')?;
        assert_insert(&["┃\"{  }\""], &["┃\"{  }\""], '{')?;
        assert_insert(&["┃\"{  }\""], &["┃\"{  }\""], '[')?;
        assert_insert(&["┃\"{  }\""], &["┃\"{  }\""], '}')?;
        assert_insert(&["┃\"{  }\""], &["┃\"{  }\""], ']')?;
        assert_insert(&["┃\"{  }\""], &["┃\"{  }\""], '-')?;

        assert_insert(&["\"{  }\"┃"], &["\"{  }\"┃"], 'a')?;
        assert_insert(&["\"{  }\"┃"], &["\"{  }\"┃"], 'A')?;
        assert_insert(&["\"{  }\"┃"], &["\"{  }\"┃"], '"')?;
        assert_insert(&["\"{  }\"┃"], &["\"{  }\"┃"], '{')?;
        assert_insert(&["\"{  }\"┃"], &["\"{  }\"┃"], '[')?;
        assert_insert(&["\"{  }\"┃"], &["\"{  }\"┃"], '}')?;
        assert_insert(&["\"{  }\"┃"], &["\"{  }\"┃"], ']')?;
        assert_insert(&["\"{  }\"┃"], &["\"{  }\"┃"], '-')?;

        assert_insert(&["\"[ 1, 2, 3 ]\"┃"], &["\"[ 1, 2, 3 ]\"┃"], '{')?;
        assert_insert(&["┃\"[ 1, 2, 3 ]\""], &["┃\"[ 1, 2, 3 ]\""], '{')?;
        assert_insert(
            &["\"hello, hello, hello\"┃"],
            &["\"hello, hello, hello\"┃"],
            '.',
        )?;
        assert_insert(
            &["┃\"hello, hello, hello\""],
            &["┃\"hello, hello, hello\""],
            '.',
        )?;

        Ok(())
    }

    #[test]
    fn test_record() -> Result<(), String> {
        assert_insert(&["┃"], &["{ ┃ }"], '{')?;
        assert_insert(&["{ ┃ }"], &["{ a┃ }"], 'a')?;
        assert_insert(&["{ a┃ }"], &["{ ab┃ }"], 'b')?;
        assert_insert(&["{ a┃ }"], &["{ a1┃ }"], '1')?;
        assert_insert(&["{ a1┃ }"], &["{ a1z┃ }"], 'z')?;
        assert_insert(&["{ a1┃ }"], &["{ a15┃ }"], '5')?;
        assert_insert(&["{ ab┃ }"], &["{ abc┃ }"], 'c')?;
        assert_insert(&["{ ┃abc }"], &["{ z┃abc }"], 'z')?;
        assert_insert(&["{ a┃b }"], &["{ az┃b }"], 'z')?;
        assert_insert(&["{ a┃b }"], &["{ a9┃b }"], '9')?;

        // extra space for Blank node
        assert_insert(&["{ a┃ }"], &["{ a: ┃  }"], ':')?;
        assert_insert(&["{ abc┃ }"], &["{ abc: ┃  }"], ':')?;
        assert_insert(&["{ aBc┃ }"], &["{ aBc: ┃  }"], ':')?;

        assert_insert_seq(&["{ a┃ }"], &["{ a: \"┃\" }"], ":\"")?;
        assert_insert_seq(&["{ abc┃ }"], &["{ abc: \"┃\" }"], ":\"")?;

        assert_insert(&["{ a: \"┃\" }"], &["{ a: \"a┃\" }"], 'a')?;
        assert_insert(&["{ a: \"a┃\" }"], &["{ a: \"ab┃\" }"], 'b')?;
        assert_insert(&["{ a: \"a┃b\" }"], &["{ a: \"az┃b\" }"], 'z')?;
        assert_insert(&["{ a: \"┃ab\" }"], &["{ a: \"z┃ab\" }"], 'z')?;

        assert_insert(&["{ camelCase: \"┃\" }"], &["{ camelCase: \"a┃\" }"], 'a')?;
        assert_insert(&["{ camelCase: \"a┃\" }"], &["{ camelCase: \"ab┃\" }"], 'b')?;

        assert_insert(&["{ a┃: \"\" }"], &["{ ab┃: \"\" }"], 'b')?;
        assert_insert(&["{ ┃a: \"\" }"], &["{ z┃a: \"\" }"], 'z')?;
        assert_insert(&["{ ab┃: \"\" }"], &["{ abc┃: \"\" }"], 'c')?;
        assert_insert(&["{ ┃ab: \"\" }"], &["{ z┃ab: \"\" }"], 'z')?;
        assert_insert(
            &["{ camelCase┃: \"hello\" }"],
            &["{ camelCaseB┃: \"hello\" }"],
            'B',
        )?;
        assert_insert(
            &["{ camel┃Case: \"hello\" }"],
            &["{ camelZ┃Case: \"hello\" }"],
            'Z',
        )?;
        assert_insert(
            &["{ ┃camelCase: \"hello\" }"],
            &["{ z┃camelCase: \"hello\" }"],
            'z',
        )?;

        assert_insert_seq(&["┃"], &["{ camelCase: \"hello┃\" }"], "{camelCase:\"hello")?;

        Ok(())
    }

    #[test]
    fn test_nested_record() -> Result<(), String> {
        assert_insert_seq(&["{ a┃ }"], &["{ a: { ┃ } }"], ":{")?;
        assert_insert_seq(&["{ abc┃ }"], &["{ abc: { ┃ } }"], ":{")?;
        assert_insert_seq(&["{ camelCase┃ }"], &["{ camelCase: { ┃ } }"], ":{")?;

        assert_insert_seq(&["{ a: { ┃ } }"], &["{ a: { zulu┃ } }"], "zulu")?;
        assert_insert_seq(
            &["{ abc: { ┃ } }"],
            &["{ abc: { camelCase┃ } }"],
            "camelCase",
        )?;
        assert_insert_seq(&["{ camelCase: { ┃ } }"], &["{ camelCase: { z┃ } }"], "z")?;

        assert_insert_seq(&["{ a: { zulu┃ } }"], &["{ a: { zulu: ┃  } }"], ":")?;
        assert_insert_seq(
            &["{ abc: { camelCase┃ } }"],
            &["{ abc: { camelCase: ┃  } }"],
            ":",
        )?;
        assert_insert_seq(
            &["{ camelCase: { z┃ } }"],
            &["{ camelCase: { z: ┃  } }"],
            ":",
        )?;

        assert_insert_seq(&["{ a┃: { zulu } }"], &["{ a0┃: { zulu } }"], "0")?;
        assert_insert_seq(
            &["{ ab┃c: { camelCase } }"],
            &["{ abz┃c: { camelCase } }"],
            "z",
        )?;
        assert_insert_seq(&["{ ┃camelCase: { z } }"], &["{ x┃camelCase: { z } }"], "x")?;

        assert_insert_seq(&["{ a: { zulu┃ } }"], &["{ a: { zulu: \"┃\" } }"], ":\"")?;
        assert_insert_seq(
            &["{ abc: { camelCase┃ } }"],
            &["{ abc: { camelCase: \"┃\" } }"],
            ":\"",
        )?;
        assert_insert_seq(
            &["{ camelCase: { z┃ } }"],
            &["{ camelCase: { z: \"┃\" } }"],
            ":\"",
        )?;

        assert_insert_seq(
            &["{ a: { zulu: \"┃\" } }"],
            &["{ a: { zulu: \"azula┃\" } }"],
            "azula",
        )?;
        assert_insert_seq(
            &["{ a: { zulu: \"az┃a\" } }"],
            &["{ a: { zulu: \"azul┃a\" } }"],
            "ul",
        )?;

        assert_insert_seq(&["{ a: { zulu┃ } }"], &["{ a: { zulu: { ┃ } } }"], ":{")?;
        assert_insert_seq(
            &["{ abc: { camelCase┃ } }"],
            &["{ abc: { camelCase: { ┃ } } }"],
            ":{",
        )?;
        assert_insert_seq(
            &["{ camelCase: { z┃ } }"],
            &["{ camelCase: { z: { ┃ } } }"],
            ":{",
        )?;

        assert_insert_seq(
            &["{ a: { zulu: { ┃ } } }"],
            &["{ a: { zulu: { he┃ } } }"],
            "he",
        )?;
        assert_insert_seq(
            &["{ a: { ┃zulu: {  } } }"],
            &["{ a: { x┃zulu: {  } } }"],
            "x",
        )?;
        assert_insert_seq(
            &["{ a: { z┃ulu: {  } } }"],
            &["{ a: { z9┃ulu: {  } } }"],
            "9",
        )?;
        assert_insert_seq(
            &["{ a: { zulu┃: {  } } }"],
            &["{ a: { zulu7┃: {  } } }"],
            "7",
        )?;

        assert_insert_seq(
            &["{ a┃: { bcD: { eFgHij: { k15 } } } }"],
            &["{ a4┃: { bcD: { eFgHij: { k15 } } } }"],
            "4",
        )?;
        assert_insert_seq(
            &["{ ┃a: { bcD: { eFgHij: { k15 } } } }"],
            &["{ y┃a: { bcD: { eFgHij: { k15 } } } }"],
            "y",
        )?;
        assert_insert_seq(
            &["{ a: { bcD: { eF┃gHij: { k15 } } } }"],
            &["{ a: { bcD: { eFxyz┃gHij: { k15 } } } }"],
            "xyz",
        )?;

        assert_insert_seq(
            &["┃"],
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase┃ } } } } } } } }"],
            "{g:{oi:{ng:{d:{e:{e:{p:{camelCase",
        )?;

        Ok(())
    }

    #[test]
    fn test_ignore_record() -> Result<(), String> {
        assert_insert_seq_ignore(&["┃{  }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{  }┃"], "a{\"5")?;
        assert_insert_seq_ignore(&["{┃  }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{  ┃}"], "a{\"5")?;

        assert_insert_seq_ignore(&["{ ┃ }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ ┃a }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ ┃abc }"], "{\"5")?;

        assert_insert_seq_ignore(&["┃{ a }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a }┃"], "a{\"5")?;
        assert_insert_seq_ignore(&["{┃ a }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a ┃}"], "a{\"5")?;

        assert_insert_seq_ignore(&["┃{ a15 }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a15 }┃"], "a{\"5")?;
        assert_insert_seq_ignore(&["{┃ a15 }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a15 ┃}"], "a{\"5")?;

        assert_insert_seq_ignore(&["┃{ camelCase }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ camelCase }┃"], "a{\"5")?;
        assert_insert_seq_ignore(&["{┃ camelCase }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ camelCase ┃}"], "a{\"5")?;

        assert_insert_seq_ignore(&["┃{ a: \"\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{┃ a: \"\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a: ┃\"\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a: \"\"┃ }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a: \"\" }┃"], "a{\"5")?;

        assert_insert_seq_ignore(&["┃{ camelCase: \"\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{┃ camelCase: \"\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ camelCase: ┃\"\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ camelCase: \"\"┃ }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ camelCase: \"\" }┃"], "a{\"5")?;

        assert_insert_seq_ignore(&["┃{ a: \"z\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{┃ a: \"z\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a: ┃\"z\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a: \"z\"┃ }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a: \"z\" }┃"], "a{\"5")?;

        assert_insert_seq_ignore(&["┃{ a: \"hello, hello.12345ZXY{}[]-><-\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{┃ a: \"hello, hello.12345ZXY{}[]-><-\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a: ┃\"hello, hello.12345ZXY{}[]-><-\" }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a: \"hello, hello.12345ZXY{}[]-><-\"┃ }"], "a{\"5")?;
        assert_insert_seq_ignore(&["{ a: \"hello, hello.12345ZXY{}[]-><-\" }┃"], "a{\"5")?;

        Ok(())
    }

    #[test]
    fn test_ignore_nested_record() -> Result<(), String> {
        assert_insert_seq_ignore(&["{ a: { ┃ } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ a: ┃{  } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ a: {┃  } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ a: {  }┃ }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ a: {  } ┃}"], "{\"5")?;
        assert_insert_seq_ignore(&["{ a: {  } }┃"], "{\"5")?;
        assert_insert_seq_ignore(&["{ a:┃ {  } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{┃ a: {  } }"], "{\"5")?;
        assert_insert_seq_ignore(&["┃{ a: {  } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ ┃a: {  } }"], "1")?;

        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a ┃} }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: {┃ z15a } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: ┃{ z15a } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a }┃ }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a } ┃}"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a } }┃"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1:┃ { z15a } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{┃ camelCaseB1: { z15a } }"], "{\"5")?;
        assert_insert_seq_ignore(&["┃{ camelCaseB1: { z15a } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ ┃camelCaseB1: { z15a } }"], "1")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { ┃z15a } }"], "1")?;

        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: \"\"┃ } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: ┃\"\" } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a:┃ \"\" } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: \"\" ┃} }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: {┃ z15a: \"\" } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: ┃{ z15a: \"\" } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: \"\" }┃ }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: \"\" } ┃}"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: \"\" } }┃"], "{\"5")?;
        assert_insert_seq_ignore(&["{ camelCaseB1:┃ { z15a: \"\" } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{┃ camelCaseB1: { z15a: \"\" } }"], "{\"5")?;
        assert_insert_seq_ignore(&["┃{ camelCaseB1: { z15a: \"\" } }"], "{\"5")?;
        assert_insert_seq_ignore(&["{ ┃camelCaseB1: { z15a: \"\" } }"], "1")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { ┃z15a: \"\" } }"], "1")?;

        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: \"hello, hello.12345ZXY{}[]-><-\"┃ } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: ┃\"hello, hello.12345ZXY{}[]-><-\" } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a:┃ \"hello, hello.12345ZXY{}[]-><-\" } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: \"hello, hello.12345ZXY{}[]-><-\" ┃} }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: {┃ z15a: \"hello, hello.12345ZXY{}[]-><-\" } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: ┃{ z15a: \"hello, hello.12345ZXY{}[]-><-\" } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: \"hello, hello.12345ZXY{}[]-><-\" }┃ }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: \"hello, hello.12345ZXY{}[]-><-\" } ┃}"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: \"hello, hello.12345ZXY{}[]-><-\" } }┃"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1:┃ { z15a: \"hello, hello.12345ZXY{}[]-><-\" } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{┃ camelCaseB1: { z15a: \"hello, hello.12345ZXY{}[]-><-\" } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["┃{ camelCaseB1: { z15a: \"hello, hello.12345ZXY{}[]-><-\" } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ ┃camelCaseB1: { z15a: \"hello, hello.12345ZXY{}[]-><-\" } }"],
            "1",
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { ┃z15a: \"hello, hello.12345ZXY{}[]-><-\" } }"],
            "1",
        )?;

        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase ┃} } } } } } } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase } ┃} } } } } } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } } }┃"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase } } } } } ┃} } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: {┃ e: { p: { camelCase } } } } } } } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: { e:┃ { p: { camelCase } } } } } } } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{┃ g: { oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["┃{ g: { oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } } }"],
            "{\"5",
        )?;
        assert_insert_seq_ignore(
            &["{ ┃g: { oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } } }"],
            "2",
        )?;
        Ok(())
    }
}
