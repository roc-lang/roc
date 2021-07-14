#![allow(dead_code)]

use crate::editor::code_lines::CodeLines;
use crate::editor::ed_error::from_ui_res;
use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::MissingSelection;
use crate::editor::grid_node_map::GridNodeMap;
use crate::editor::markup::attribute::Attributes;
use crate::editor::markup::nodes;
use crate::editor::markup::nodes::MarkupNode;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_model::SelectedExpression;
use crate::editor::mvc::int_update::start_new_int;
use crate::editor::mvc::int_update::update_int;
use crate::editor::mvc::list_update::{add_blank_child, start_new_list};
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
use crate::lang::constrain::constrain_expr;
use crate::lang::pool::NodeId;
use crate::lang::pool::Pool;
use crate::lang::pool::PoolStr;
use crate::lang::types::Type2;
use crate::lang::{constrain::Constraint, solve};
use crate::ui::text::caret_w_select::CaretWSelect;
use crate::ui::text::lines::MoveCaretFun;
use crate::ui::text::selection::validate_raw_sel;
use crate::ui::text::selection::RawSelection;
use crate::ui::text::selection::Selection;
use crate::ui::text::text_pos::TextPos;
use crate::ui::text::{lines, lines::Lines, lines::SelectableLines};
use crate::ui::ui_error::UIResult;
use crate::window::keyboard_input::Modifiers;
use bumpalo::Bump;
use roc_can::expected::Expected;
use roc_collections::all::MutMap;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use roc_types::solved_types::Solved;
use roc_types::subs::{Subs, Variable};
use roc_types::{pretty_print::content_to_string, subs::VarStore};
use snafu::OptionExt;
use winit::event::VirtualKeyCode;
use VirtualKeyCode::*;

use super::let_update::start_new_let_value;
use super::let_update::update_let_value;

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
        self.selected_expr_opt = None;

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

    // disregards EdModel.code_lines because the caller knows the resulting caret position will be valid.
    // allows us to prevent multiple updates to EdModel.code_lines
    pub fn simple_move_carets_left(&mut self, repeat: usize) {
        for caret_tup in self.caret_w_select_vec.iter_mut() {
            caret_tup.0.caret_pos.column -= repeat;
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

    pub fn add_mark_node(&mut self, node: MarkupNode) -> MarkNodeId {
        self.markup_node_pool.add(node)
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
            let node_content_str = node.get_content();

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
            let node_content_str = node.get_content();

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

    pub fn insert_all_between_line(
        &mut self,
        line_nr: usize,
        index: usize,
        node_ids: &[MarkNodeId],
    ) -> UIResult<()> {

        let mut col_nr = index;

        for &node_id in node_ids {
            let node_content_str = self.markup_node_pool.get(node_id).get_content();

            self.grid_node_map
            .insert_between_line(line_nr, col_nr, node_content_str.len(), node_id)?;

            self.code_lines.insert_between_line(line_nr, col_nr, &node_content_str)?;

            col_nr += node_content_str.len();
        }

        Ok(())
    }

    // updates grid_node_map and code_lines but nothing else.
    pub fn del_at_line(&mut self, line_nr: usize, index: usize) -> UIResult<()> {
        self.grid_node_map.del_at_line(line_nr, index)?;
        self.code_lines.del_at_line(line_nr, index)
    }

    pub fn set_selected_expr(
        &mut self,
        expr_start_pos: TextPos,
        expr_end_pos: TextPos,
        ast_node_id: NodeId<Expr2>,
        mark_node_id: MarkNodeId,
    ) -> EdResult<()> {
        self.set_raw_sel(RawSelection {
            start_pos: expr_start_pos,
            end_pos: expr_end_pos,
        })?;

        self.set_caret(expr_start_pos);

        let type_str = self.expr2_to_type(ast_node_id);

        self.selected_expr_opt = Some(SelectedExpression {
            ast_node_id,
            mark_node_id,
            type_str,
        });

        self.dirty = true;

        Ok(())
    }

    // select all MarkupNodes that refer to specific ast node and its children.
    pub fn select_expr(&mut self) -> EdResult<()> {
        // include parent in selection if an `Expr2` was already selected
        if let Some(selected_expr) = &self.selected_expr_opt {
            let expr2_level_mark_node = self.markup_node_pool.get(selected_expr.mark_node_id);

            if let Some(parent_id) = expr2_level_mark_node.get_parent_id_opt() {
                let parent_mark_node = self.markup_node_pool.get(parent_id);
                let ast_node_id = parent_mark_node.get_ast_node_id();

                let (expr_start_pos, expr_end_pos) = self
                    .grid_node_map
                    .get_nested_start_end_pos(parent_id, self)?;

                self.set_selected_expr(expr_start_pos, expr_end_pos, ast_node_id, parent_id)?;
            }
        } else {
            // select `Expr2` in which caret is currently positioned
            let caret_pos = self.get_caret();
            if self.grid_node_map.node_exists_at_pos(caret_pos) {
                let (expr_start_pos, expr_end_pos, ast_node_id, mark_node_id) = self
                    .grid_node_map
                    .get_expr_start_end_pos(self.get_caret(), &self)?;

                self.set_selected_expr(expr_start_pos, expr_end_pos, ast_node_id, mark_node_id)?;
            } else if self
                .grid_node_map
                .node_exists_at_pos(caret_pos.decrement_col())
            {
                let (expr_start_pos, expr_end_pos, ast_node_id, mark_node_id) = self
                    .grid_node_map
                    .get_expr_start_end_pos(self.get_caret().decrement_col(), &self)?;

                self.set_selected_expr(expr_start_pos, expr_end_pos, ast_node_id, mark_node_id)?;
            }
        }

        Ok(())
    }

    fn expr2_to_type(&mut self, expr2_id: NodeId<Expr2>) -> PoolStr {
        let var = self.module.env.var_store.fresh();
        let expr = self.module.env.pool.get(expr2_id);
        let arena = Bump::new();

        let constrained = constrain_expr(
            &arena,
            &mut self.module.env,
            &expr,
            Expected::NoExpectation(Type2::Variable(var)),
            Region::zero(),
        );

        // extract the var_store out of the env
        let mut var_store = VarStore::default();
        std::mem::swap(self.module.env.var_store, &mut var_store);

        let (mut solved, _, _) = EdModel::run_solve(
            self.module.env.pool,
            Default::default(),
            Default::default(),
            constrained,
            var_store,
        );

        // put the updated var_store back in env
        std::mem::swap(
            &mut VarStore::new_from_subs(solved.inner()),
            self.module.env.var_store,
        );

        let subs = solved.inner_mut();

        let content = subs.get(var).content;

        PoolStr::new(
            &content_to_string(content, &subs, self.module.env.home, self.interns),
            self.module.env.pool,
        )
    }

    fn run_solve(
        mempool: &mut Pool,
        aliases: MutMap<Symbol, roc_types::types::Alias>,
        rigid_variables: MutMap<Variable, Lowercase>,
        constraint: Constraint,
        var_store: VarStore,
    ) -> (Solved<Subs>, solve::Env, Vec<solve::TypeError>) {
        let env = solve::Env {
            vars_by_symbol: MutMap::default(),
            aliases,
        };
        let arena = Bump::new();

        let mut subs = Subs::new(var_store);

        for (var, name) in rigid_variables {
            subs.rigid_var(var, name);
        }

        // Now that the module is parsed, canonicalized, and constrained,
        // we need to type check it.
        let mut problems = Vec::new();

        // Run the solver to populate Subs.
        let (solved_subs, solved_env) =
            solve::run(&arena, mempool, &env, &mut problems, subs, &constraint);

        (solved_subs, solved_env, problems)
    }

    pub fn ed_handle_key_down(
        &mut self,
        modifiers: &Modifiers,
        virtual_keycode: VirtualKeyCode,
    ) -> EdResult<()> {
        match virtual_keycode {
            Left => from_ui_res(self.move_caret_left(modifiers)),
            Up => {
                if modifiers.cmd_or_ctrl() && modifiers.shift {
                    self.select_expr()
                } else {
                    from_ui_res(self.move_caret_up(modifiers))
                }
            }
            Right => from_ui_res(self.move_caret_right(modifiers)),
            Down => from_ui_res(self.move_caret_down(modifiers)),

            A => {
                if modifiers.cmd_or_ctrl() {
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

    fn replace_selected_expr_with_blank(&mut self) -> EdResult<()> {
        let expr_mark_node_id_opt = if let Some(sel_expr) = &self.selected_expr_opt {
            let expr2_level_mark_node = self.markup_node_pool.get(sel_expr.mark_node_id);

            let blank_replacement = MarkupNode::Blank {
                ast_node_id: sel_expr.ast_node_id,
                attributes: Attributes::new(),
                syn_high_style: HighlightStyle::Blank,
                parent_id_opt: expr2_level_mark_node.get_parent_id_opt(),
            };

            self.markup_node_pool
                .replace_node(sel_expr.mark_node_id, blank_replacement);

            let active_selection = self.get_selection().context(MissingSelection {})?;

            self.code_lines.del_selection(active_selection)?;
            self.grid_node_map.del_selection(active_selection)?;

            self.module.env.pool.set(sel_expr.ast_node_id, Expr2::Blank);

            Some(sel_expr.mark_node_id)
        } else {
            None
        };

        // have to split the previous `if` up to prevent borrowing issues
        if let Some(expr_mark_node_id) = expr_mark_node_id_opt {
            let caret_pos = self.get_caret();

            self.insert_between_line(
                caret_pos.line,
                caret_pos.column,
                nodes::BLANK_PLACEHOLDER,
                expr_mark_node_id,
            )?;
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
        self.selected_expr_opt = None;
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

                ed_model.replace_selected_expr_with_blank()?;

                InputOutcome::Accepted
            }
            ch => {
                let outcome =
                    if ed_model.node_exists_at_caret() {
                        let curr_mark_node_id = ed_model.get_curr_mark_node_id()?;
                            let curr_mark_node = ed_model.markup_node_pool.get(curr_mark_node_id);
                            let prev_mark_node_id_opt = ed_model.get_prev_mark_node_id()?;

                            let ast_node_id = curr_mark_node.get_ast_node_id();
                            let ast_node_ref = ed_model.module.env.pool.get(ast_node_id);

                            if let Expr2::Blank {..} = ast_node_ref {
                                match ch {
                                    'a'..='z' => {
                                        start_new_let_value(ed_model, ch)?
                                    }
                                    '"' => {
                                        start_new_string(ed_model)?
                                    },
                                    '{' => {
                                        start_new_record(ed_model)?
                                    }
                                    '0'..='9' => {
                                        start_new_int(ed_model, ch)?
                                    }
                                    '[' => {
                                        // this can also be a tag union or become a set, assuming list for now
                                        start_new_list(ed_model)?
                                    }
                                    _ => InputOutcome::Ignored
                                }
                            } else if let Some(prev_mark_node_id) = prev_mark_node_id_opt{
                                if prev_mark_node_id == curr_mark_node_id {
                                    match ast_node_ref {
                                        Expr2::SmallInt{ .. } => {
                                            update_int(ed_model, curr_mark_node_id, ch)?
                                        }
                                        Expr2::SmallStr(old_arr_str) => {
                                            update_small_string(
                                                &ch, old_arr_str, ed_model
                                            )?
                                        }
                                        Expr2::Str(..) => {
                                            update_string(*ch, ed_model)?
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
                                            if curr_mark_node.get_content().chars().all(|chr| chr.is_ascii_alphanumeric()){
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

                                    match ast_node_ref {
                                        Expr2::SmallInt{ .. } => {
                                            update_int(ed_model, curr_mark_node_id, ch)?
                                        }
                                        _ => {
                                            let prev_ast_node_id =
                                                ed_model
                                                .markup_node_pool
                                                .get(prev_mark_node_id)
                                                .get_ast_node_id();

                                            let prev_node_ref = ed_model.module.env.pool.get(prev_ast_node_id);

                                            match prev_node_ref {
                                                Expr2::SmallInt{ .. } => {
                                                    update_int(ed_model, prev_mark_node_id, ch)?
                                                }
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

                                                    if (curr_mark_node.get_content() == nodes::RIGHT_ACCOLADE || curr_mark_node.get_content() == nodes::COLON) &&
                                                        prev_mark_node.is_all_alphanumeric() {
                                                        update_record_field(
                                                            &ch.to_string(),
                                                            ed_model.get_caret(),
                                                            prev_mark_node_id,
                                                            fields,
                                                            ed_model,
                                                        )?
                                                    } else if prev_mark_node.get_content() == nodes::LEFT_ACCOLADE && curr_mark_node.is_all_alphanumeric() {
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
                                                Expr2::List{ elem_var: _, elems: _} => {
                                                    let prev_mark_node = ed_model.markup_node_pool.get(prev_mark_node_id);

                                                    if prev_mark_node.get_content() == nodes::LEFT_SQUARE_BR && curr_mark_node.get_content() == nodes::RIGHT_SQUARE_BR {
                                                        // based on if, we are at the start of the list
                                                        let new_child_index = 1;
                                                        let new_ast_child_index = 0;
                                                        // insert a Blank first, this results in cleaner code
                                                        add_blank_child(new_child_index, new_ast_child_index, ed_model)?;
                                                        handle_new_char(received_char, ed_model)?
                                                    } else {
                                                        InputOutcome::Ignored
                                                    }
                                                }
                                                Expr2::LetValue{ def_id, .. } => {
                                                    update_let_value(prev_mark_node_id, *def_id, ed_model, ch)?
                                                }
                                                _ => {
                                                    match ast_node_ref {
                                                        Expr2::EmptyRecord => {
                                                            let sibling_ids = curr_mark_node.get_sibling_ids(&ed_model.markup_node_pool);

                                                            update_empty_record(
                                                                &ch.to_string(),
                                                                prev_mark_node_id,
                                                                sibling_ids,
                                                                ed_model
                                                            )?
                                                        }
                                                        _ => InputOutcome::Ignored
                                                    }
                                                }
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
                                } else if *ch == ',' {
                                    if curr_mark_node.get_content() == nodes::LEFT_SQUARE_BR {
                                        InputOutcome::Ignored
                                    } else {
                                        let mark_parent_id_opt = curr_mark_node.get_parent_id_opt();

                                        if let Some(mark_parent_id) = mark_parent_id_opt {
                                            let parent_ast_id = ed_model.markup_node_pool.get(mark_parent_id).get_ast_node_id();
                                            let parent_expr2 = ed_model.module.env.pool.get(parent_ast_id);

                                            match parent_expr2 {
                                                Expr2::List { elem_var:_, elems:_} => {

                                                    let (new_child_index, new_ast_child_index) = ed_model.get_curr_child_indices()?;
                                                    // insert a Blank first, this results in cleaner code
                                                    add_blank_child(
                                                        new_child_index,
                                                        new_ast_child_index,
                                                        ed_model
                                                    )?
                                                }
                                                Expr2::Record { record_var:_, fields:_ } => {
                                                    todo!("multiple record fields")
                                                }
                                                _ => {
                                                    InputOutcome::Ignored
                                                }
                                            }
                                        } else {
                                            InputOutcome::Ignored
                                        }
                                    }
                                } else if "\"{[".contains(*ch) {
                                    let prev_mark_node = ed_model.markup_node_pool.get(prev_mark_node_id);

                                    if prev_mark_node.get_content() == nodes::LEFT_SQUARE_BR && curr_mark_node.get_content() == nodes::RIGHT_SQUARE_BR {
                                        let (new_child_index, new_ast_child_index) = ed_model.get_curr_child_indices()?;
                                        // insert a Blank first, this results in cleaner code
                                        add_blank_child(
                                            new_child_index,
                                            new_ast_child_index,
                                            ed_model
                                        )?;
                                        handle_new_char(received_char, ed_model)?
                                    } else {
                                        InputOutcome::Ignored
                                    }

                                } else {
                                    InputOutcome::Ignored
                                }

                            } else {
                                match ast_node_ref {
                                    Expr2::SmallInt{ .. } => {
                                        update_int(ed_model, curr_mark_node_id, ch)?
                                    },
                                    // only SmallInt currently allows prepending at the start
                                    _ => InputOutcome::Ignored
                                }
                            }

                    } else { //no MarkupNode at the current position
                            let prev_mark_node_id_opt = ed_model.get_prev_mark_node_id()?;
                            if let Some(prev_mark_node_id) = prev_mark_node_id_opt {
                                let prev_mark_node = ed_model.markup_node_pool.get(prev_mark_node_id);

                                let prev_ast_node = ed_model.module.env.pool.get(prev_mark_node.get_ast_node_id());

                                match prev_ast_node {
                                    Expr2::SmallInt{ .. } => {
                                        update_int(ed_model, prev_mark_node_id, ch)?
                                    },
                                    _ => {
                                        InputOutcome::Ignored
                                    }
                                }
                            } else {
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
    use crate::editor::mvc::ed_update::EdModel;
    use crate::editor::mvc::ed_update::EdResult;
    use crate::ui::text::lines::SelectableLines;
    use crate::ui::ui_error::UIResult;
    use crate::window::keyboard_input::no_mods;
    use crate::window::keyboard_input::test_modifiers::ctrl_cmd_shift;
    use crate::window::keyboard_input::Modifiers;
    use bumpalo::collections::String as BumpString;
    use bumpalo::Bump;
    use winit::event::VirtualKeyCode::*;

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

    // Create ed_model from pre_lines DSL, do handle_new_char() with new_char, check if modified ed_model has expected
    // string representation of code, caret position and active selection.
    pub fn assert_insert(
        pre_lines: &[&str],
        expected_post_lines: &[&str],
        new_char: char,
    ) -> Result<(), String> {
        assert_insert_seq(pre_lines, expected_post_lines, &new_char.to_string())
    }

    pub fn assert_insert_ignore(lines: &[&str], new_char: char) -> Result<(), String> {
        assert_insert_seq(lines, lines, &new_char.to_string())
    }

    // Create ed_model from pre_lines DSL, do handle_new_char() for every char in new_char_seq, check if modified ed_model has expected
    // string representation of code, caret position and active selection.
    pub fn assert_insert_seq(
        pre_lines: &[&str],
        expected_post_lines: &[&str],
        new_char_seq: &str,
    ) -> Result<(), String> {
        let test_arena = Bump::new();
        let code_str = BumpString::from_str_in(&pre_lines.join("\n").replace("┃", ""), &test_arena);

        let mut model_refs = init_model_refs();

        let mut ed_model = ed_model_from_dsl(&code_str, pre_lines, &mut model_refs)?;

        for input_char in new_char_seq.chars() {
            if input_char == '🡲' {
                ed_model.simple_move_carets_right(1);
            } else if input_char == '🡰' {
                ed_model.simple_move_carets_left(1);
            } else {
                ed_res_to_res(handle_new_char(&input_char, &mut ed_model))?;
            }
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
        assert_insert(&["┃"], &["┃ "], '-')?;
        assert_insert(&["┃"], &["┃ "], '_')?;

        Ok(())
    }

    #[test]
    fn test_int() -> Result<(), String> {
        assert_insert(&["┃"], &["0┃"], '0')?;
        assert_insert(&["┃"], &["1┃"], '1')?;
        assert_insert(&["┃"], &["2┃"], '2')?;
        assert_insert(&["┃"], &["3┃"], '3')?;
        assert_insert(&["┃"], &["4┃"], '4')?;
        assert_insert(&["┃"], &["5┃"], '5')?;
        assert_insert(&["┃"], &["6┃"], '6')?;
        assert_insert(&["┃"], &["7┃"], '7')?;
        assert_insert(&["┃"], &["8┃"], '8')?;
        assert_insert(&["┃"], &["9┃"], '9')?;

        assert_insert(&["1┃"], &["19┃"], '9')?;
        assert_insert(&["9876┃"], &["98769┃"], '9')?;
        assert_insert(&["10┃"], &["103┃"], '3')?;
        assert_insert(&["┃0"], &["1┃0"], '1')?;
        assert_insert(&["10000┃"], &["100000┃"], '0')?;

        assert_insert(&["┃1234"], &["5┃1234"], '5')?;
        assert_insert(&["1┃234"], &["10┃234"], '0')?;
        assert_insert(&["12┃34"], &["121┃34"], '1')?;
        assert_insert(&["123┃4"], &["1232┃4"], '2')?;

        Ok(())
    }

    #[test]
    fn test_ignore_int() -> Result<(), String> {
        assert_insert_seq_ignore(&["┃0"], "{}()[]-><-_\"azAZ:@")?;
        assert_insert_seq_ignore(&["┃7"], "{}()[]-><-_\"azAZ:@")?;

        assert_insert_seq_ignore(&["0┃"], ",{}()[]-><-_\"azAZ:@")?;
        assert_insert_seq_ignore(&["8┃"], ",{}()[]-><-_\"azAZ:@")?;
        assert_insert_seq_ignore(&["20┃"], ",{}()[]-><-_\"azAZ:@")?;
        assert_insert_seq_ignore(&["83┃"], ",{}()[]-><-_\"azAZ:@")?;

        assert_insert_seq_ignore(&["1┃0"], ",{}()[]-><-_\"azAZ:@")?;
        assert_insert_seq_ignore(&["8┃4"], ",{}()[]-><-_\"azAZ:@")?;

        assert_insert_seq_ignore(&["┃10"], ",{}()[]-><-_\"azAZ:@")?;
        assert_insert_seq_ignore(&["┃84"], ",{}()[]-><-_\"azAZ:@")?;

        assert_insert_seq_ignore(&["129┃96"], ",{}()[]-><-_\"azAZ:@")?;
        assert_insert_seq_ignore(&["97┃684"], ",{}()[]-><-_\"azAZ:@")?;

        assert_insert_ignore(&["0┃"], '0')?;
        assert_insert_ignore(&["0┃"], '9')?;
        assert_insert_ignore(&["┃0"], '0')?;
        assert_insert_ignore(&["┃1234"], '0')?;
        assert_insert_ignore(&["┃100"], '0')?;

        Ok(())
    }

    //TODO test_int arch bit limit

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
        assert_insert(&["{ a┃ }"], &["{ ab┃: RunTimeError }"], 'b')?;
        assert_insert(&["{ a┃ }"], &["{ a1┃: RunTimeError }"], '1')?;
        assert_insert(&["{ a1┃ }"], &["{ a1z┃: RunTimeError }"], 'z')?;
        assert_insert(&["{ a1┃ }"], &["{ a15┃: RunTimeError }"], '5')?;
        assert_insert(&["{ ab┃ }"], &["{ abc┃: RunTimeError }"], 'c')?;
        assert_insert(&["{ ┃abc }"], &["{ z┃abc: RunTimeError }"], 'z')?;
        assert_insert(&["{ a┃b }"], &["{ az┃b: RunTimeError }"], 'z')?;
        assert_insert(&["{ a┃b }"], &["{ a9┃b: RunTimeError }"], '9')?;

        // extra space for Blank node
        assert_insert(&["{ a┃ }"], &["{ a┃: RunTimeError }"], ':')?;
        assert_insert(&["{ abc┃ }"], &["{ abc┃: RunTimeError }"], ':')?;
        assert_insert(&["{ aBc┃ }"], &["{ aBc┃: RunTimeError }"], ':')?;

        assert_insert_seq(&["{ a┃ }"], &["{ a┃: RunTimeError }"], ":\"")?;
        assert_insert_seq(&["{ abc┃ }"], &["{ abc┃: RunTimeError }"], ":\"")?;

        assert_insert_seq(&["{ a┃ }"], &["{ a0┃: RunTimeError }"], ":0")?;
        assert_insert_seq(&["{ abc┃ }"], &["{ abc9┃: RunTimeError }"], ":9")?;
        assert_insert_seq(&["{ a┃ }"], &["{ a1000┃: RunTimeError }"], ":1000")?;
        assert_insert_seq(&["{ abc┃ }"], &["{ abc98761┃: RunTimeError }"], ":98761")?;

        assert_insert(&["{ a: \"┃\" }"], &["{ a: \"a┃\" }"], 'a')?;
        assert_insert(&["{ a: \"a┃\" }"], &["{ a: \"ab┃\" }"], 'b')?;
        assert_insert(&["{ a: \"a┃b\" }"], &["{ a: \"az┃b\" }"], 'z')?;
        assert_insert(&["{ a: \"┃ab\" }"], &["{ a: \"z┃ab\" }"], 'z')?;

        assert_insert(&["{ a: 1┃ }"], &["{ a: 10┃ }"], '0')?;
        assert_insert(&["{ a: 100┃ }"], &["{ a: 1004┃ }"], '4')?;
        assert_insert(&["{ a: 9┃76 }"], &["{ a: 98┃76 }"], '8')?;
        assert_insert(&["{ a: 4┃691 }"], &["{ a: 40┃691 }"], '0')?;
        assert_insert(&["{ a: 469┃1 }"], &["{ a: 4699┃1 }"], '9')?;

        assert_insert(&["{ camelCase: \"┃\" }"], &["{ camelCase: \"a┃\" }"], 'a')?;
        assert_insert(&["{ camelCase: \"a┃\" }"], &["{ camelCase: \"ab┃\" }"], 'b')?;

        assert_insert(&["{ camelCase: 3┃ }"], &["{ camelCase: 35┃ }"], '5')?;
        assert_insert(&["{ camelCase: ┃2 }"], &["{ camelCase: 5┃2 }"], '5')?;
        assert_insert(&["{ camelCase: 10┃2 }"], &["{ camelCase: 106┃2 }"], '6')?;

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

        assert_insert(&["{ a┃: 0 }"], &["{ ab┃: 0 }"], 'b')?;
        assert_insert(&["{ ┃a: 2100 }"], &["{ z┃a: 2100 }"], 'z')?;
        assert_insert(&["{ ab┃: 9876 }"], &["{ abc┃: 9876 }"], 'c')?;
        assert_insert(&["{ ┃ab: 102 }"], &["{ z┃ab: 102 }"], 'z')?;
        assert_insert(&["{ camelCase┃: 99999 }"], &["{ camelCaseB┃: 99999 }"], 'B')?;
        assert_insert(&["{ camel┃Case: 88156 }"], &["{ camelZ┃Case: 88156 }"], 'Z')?;
        assert_insert(&["{ ┃camelCase: 1 }"], &["{ z┃camelCase: 1 }"], 'z')?;

        assert_insert_seq(&["┃"], &["{ camelCase: \"hello┃\" }"], "{camelCase:\"hello")?;
        assert_insert_seq(&["┃"], &["{ camelCase: 10009┃ }"], "{camelCase:10009")?;

        Ok(())
    }

    #[test]
    fn test_nested_record() -> Result<(), String> {
        assert_insert_seq(&["{ a┃ }"], &["{ a┃: RunTimeError }"], ":{")?;
        assert_insert_seq(&["{ abc┃ }"], &["{ abc┃: RunTimeError }"], ":{")?;
        assert_insert_seq(&["{ camelCase┃ }"], &["{ camelCase┃: RunTimeError }"], ":{")?;

        assert_insert_seq(&["{ a: { ┃ } }"], &["{ a: { zulu┃ } }"], "zulu")?;
        assert_insert_seq(
            &["{ abc: { ┃ } }"],
            &["{ abc: { camelCase┃ } }"],
            "camelCase",
        )?;
        assert_insert_seq(&["{ camelCase: { ┃ } }"], &["{ camelCase: { z┃ } }"], "z")?;

        assert_insert_seq(
            &["{ a: { zulu┃ } }"],
            &["{ a: { zulu┃: RunTimeError } }"],
            ":",
        )?;
        assert_insert_seq(
            &["{ abc: { camelCase┃ } }"],
            &["{ abc: { camelCase┃: RunTimeError } }"],
            ":",
        )?;
        assert_insert_seq(
            &["{ camelCase: { z┃ } }"],
            &["{ camelCase: { z┃: RunTimeError } }"],
            ":",
        )?;

        assert_insert_seq(
            &["{ a┃: { zulu } }"],
            &["{ a0┃: { zulu: RunTimeError } }"],
            "0",
        )?;
        assert_insert_seq(
            &["{ ab┃c: { camelCase } }"],
            &["{ abz┃c: { camelCase: RunTimeError } }"],
            "z",
        )?;
        assert_insert_seq(
            &["{ ┃camelCase: { z } }"],
            &["{ x┃camelCase: { z: RunTimeError } }"],
            "x",
        )?;

        assert_insert_seq(
            &["{ a: { zulu┃ } }"],
            &["{ a: { zulu┃: RunTimeError } }"],
            ":\"",
        )?;
        assert_insert_seq(
            &["{ abc: { camelCase┃ } }"],
            &["{ abc: { camelCase┃: RunTimeError } }"],
            ":\"",
        )?;
        assert_insert_seq(
            &["{ camelCase: { z┃ } }"],
            &["{ camelCase: { z┃: RunTimeError } }"],
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

        assert_insert_seq(
            &["{ a: { zulu┃ } }"],
            &["{ a: { zulu1┃: RunTimeError } }"],
            ":1",
        )?;
        assert_insert_seq(
            &["{ abc: { camelCase┃ } }"],
            &["{ abc: { camelCase0┃: RunTimeError } }"],
            ":0",
        )?;
        assert_insert_seq(
            &["{ camelCase: { z┃ } }"],
            &["{ camelCase: { z45┃: RunTimeError } }"],
            ":45",
        )?;

        assert_insert_seq(&["{ a: { zulu: ┃0 } }"], &["{ a: { zulu: 4┃0 } }"], "4")?;
        assert_insert_seq(
            &["{ a: { zulu: 10┃98 } }"],
            &["{ a: { zulu: 1077┃98 } }"],
            "77",
        )?;

        assert_insert_seq(
            &["{ a: { zulu┃ } }"],
            &["{ a: { zulu┃: RunTimeError } }"],
            ":{",
        )?;
        assert_insert_seq(
            &["{ abc: { camelCase┃ } }"],
            &["{ abc: { camelCase┃: RunTimeError } }"],
            ":{",
        )?;
        assert_insert_seq(
            &["{ camelCase: { z┃ } }"],
            &["{ camelCase: { z┃: RunTimeError } }"],
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
            &["{ a4┃: { bcD: { eFgHij: { k15: RunTimeError } } } }"],
            "4",
        )?;
        assert_insert_seq(
            &["{ ┃a: { bcD: { eFgHij: { k15 } } } }"],
            &["{ y┃a: { bcD: { eFgHij: { k15: RunTimeError } } } }"],
            "y",
        )?;
        assert_insert_seq(
            &["{ a: { bcD: { eF┃gHij: { k15 } } } }"],
            &["{ a: { bcD: { eFxyz┃gHij: { k15: RunTimeError } } } }"],
            "xyz",
        )?;

        assert_insert_seq(
            &["┃"],
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase┃ } } } } } } } }"],
            "{g:{oi:{ng:{d:{e:{e:{p:{camelCase",
        )?;

        Ok(())
    }

    const IGNORE_CHARS: &str = "{}()[]-><-_\"azAZ:@09";
    const IGNORE_NO_LTR: &str = "{\"5";
    const IGNORE_NO_NUM: &str = "a{\"";

    #[test]
    fn test_ignore_record() -> Result<(), String> {
        assert_insert_seq_ignore(&["┃{  }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{  }┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{┃  }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{  ┃}"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["{ ┃ }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ ┃a: RunTimeError }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ ┃abc: RunTimeError }"], IGNORE_NO_LTR)?;

        assert_insert_seq_ignore(&["┃{ a: RunTimeError }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a: ┃RunTimeError }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{┃ a: RunTimeError }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a:┃ RunTimeError }"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃{ a15: RunTimeError }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a15: ┃RunTimeError }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{┃ a15: RunTimeError }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a15:┃ RunTimeError }"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃{ camelCase: RunTimeError }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ camelCase: ┃RunTimeError }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{┃ camelCase: RunTimeError }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ camelCase:┃ RunTimeError }"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃{ a: \"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{┃ a: \"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a: ┃\"\" }"], "0")?;
        assert_insert_seq_ignore(&["{ a: ┃\"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a: \"\"┃ }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a: \"\" }┃"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃{ a: 1 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{┃ a: 2 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a: ┃6 }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ a: 8┃ }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ a: 0 }┃"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃{ camelCase: 1 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{┃ camelCase: 7 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ camelCase: ┃2 }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ camelCase: 4┃ }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ camelCase: 9 }┃"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃{ camelCase: \"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{┃ camelCase: \"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ camelCase: ┃\"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ camelCase: \"\"┃ }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ camelCase: \"\" }┃"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃{ a: \"z\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{┃ a: \"z\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a: ┃\"z\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a: \"z\"┃ }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a: \"z\" }┃"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(
            &["┃{ a: \"hello, hello.0123456789ZXY{}[]-><-\" }"],
            IGNORE_CHARS,
        )?;
        assert_insert_seq_ignore(
            &["{┃ a: \"hello, hello.0123456789ZXY{}[]-><-\" }"],
            IGNORE_CHARS,
        )?;
        assert_insert_seq_ignore(
            &["{ a: ┃\"hello, hello.0123456789ZXY{}[]-><-\" }"],
            IGNORE_CHARS,
        )?;
        assert_insert_seq_ignore(
            &["{ a: \"hello, hello.0123456789ZXY{}[]-><-\"┃ }"],
            IGNORE_CHARS,
        )?;
        assert_insert_seq_ignore(
            &["{ a: \"hello, hello.0123456789ZXY{}[]-><-\" }┃"],
            IGNORE_CHARS,
        )?;

        assert_insert_seq_ignore(&["┃{ a: 915480 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{┃ a: 915480 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["{ a: ┃915480 }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ a: 915480┃ }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ a: 915480 }┃"], IGNORE_CHARS)?;

        Ok(())
    }

    #[test]
    fn test_ignore_nested_record() -> Result<(), String> {
        assert_insert_seq_ignore(&["{ a: { ┃ } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ a: ┃{  } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ a: {┃  } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ a: {  }┃ }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ a: {  } ┃}"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ a: {  } }┃"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ a:┃ {  } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{┃ a: {  } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["┃{ a: {  } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ ┃a: {  } }"], "1")?;

        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a:┃ RunTimeError } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: {┃ z15a: RunTimeError } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: ┃{ z15a: RunTimeError } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: ┃RunTimeError } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: R┃unTimeError } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: Ru┃nTimeError } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1:┃ { z15a: RunTimeError } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{┃ camelCaseB1: { z15a: RunTimeError } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["┃{ camelCaseB1: { z15a: RunTimeError } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ ┃camelCaseB1: { z15a: RunTimeError } }"], "1")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { ┃z15a: RunTimeError } }"], "1")?;

        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: \"\"┃ } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: ┃\"\" } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a:┃ \"\" } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: \"\" ┃} }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: {┃ z15a: \"\" } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: ┃{ z15a: \"\" } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: \"\" }┃ }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: \"\" } ┃}"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: \"\" } }┃"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ camelCaseB1:┃ { z15a: \"\" } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{┃ camelCaseB1: { z15a: \"\" } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["┃{ camelCaseB1: { z15a: \"\" } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore(&["{ ┃camelCaseB1: { z15a: \"\" } }"], "1")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { ┃z15a: \"\" } }"], "1")?;

        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: 0┃ } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: ┃123 } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a:┃ 999 } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: 80 ┃} }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: {┃ z15a: 99000 } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: ┃{ z15a: 12 } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: 7 }┃ }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: 98 } ┃}"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { z15a: 4582 } }┃"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ camelCaseB1:┃ { z15a: 0 } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{┃ camelCaseB1: { z15a: 44 } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["┃{ camelCaseB1: { z15a: 100123 } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["{ ┃camelCaseB1: { z15a: 5 } }"], "1")?;
        assert_insert_seq_ignore(&["{ camelCaseB1: { ┃z15a: 6 } }"], "1")?;

        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\"┃ } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: ┃\"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a:┃ \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" ┃} }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: {┃ z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: ┃{ z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" }┃ }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } ┃}"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }┃"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1:┃ { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{┃ camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["┃{ camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ ┃camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            "1",
        )?;
        assert_insert_seq_ignore(
            &["{ camelCaseB1: { ┃z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            "1",
        )?;

        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase:┃ RunTimeError } } } } } } } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase: R┃unTimeError } } } } } } } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase: RunTimeError } } } } } } } }┃"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase: RunTimeEr┃ror } } } } } } } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: {┃ e: { p: { camelCase: RunTimeError } } } } } } } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ g: { oi: { ng: { d: { e: { e:┃ { p: { camelCase: RunTimeError } } } } } } } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{┃ g: { oi: { ng: { d: { e: { e: { p: { camelCase: RunTimeError } } } } } } } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["┃{ g: { oi: { ng: { d: { e: { e: { p: { camelCase: RunTimeError } } } } } } } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore(
            &["{ ┃g: { oi: { ng: { d: { e: { e: { p: { camelCase: RunTimeError } } } } } } } }"],
            "2",
        )?;
        Ok(())
    }

    #[test]
    fn test_single_elt_list() -> Result<(), String> {
        assert_insert(&["┃"], &["[ ┃ ]"], '[')?;

        assert_insert_seq(&["┃"], &["[ 0┃ ]"], "[0")?;
        assert_insert_seq(&["┃"], &["[ 1┃ ]"], "[1")?;
        assert_insert_seq(&["┃"], &["[ 9┃ ]"], "[9")?;

        assert_insert_seq(&["┃"], &["[ \"┃\" ]"], "[\"")?;
        assert_insert_seq(
            &["┃"],
            &["[ \"hello, hello.0123456789ZXY{}[]-><-┃\" ]"],
            "[\"hello, hello.0123456789ZXY{}[]-><-",
        )?;

        assert_insert_seq(&["┃"], &["[ { ┃ } ]"], "[{")?;
        assert_insert_seq(&["┃"], &["[ { a┃ } ]"], "[{a")?;
        assert_insert_seq(
            &["┃"],
            &["[ { camelCase: { zulu: \"nested┃\" } } ]"],
            "[{camelCase:{zulu:\"nested",
        )?;

        assert_insert_seq(&["┃"], &["[ [ ┃ ] ]"], "[[")?;
        assert_insert_seq(&["┃"], &["[ [ [ ┃ ] ] ]"], "[[[")?;
        assert_insert_seq(&["┃"], &["[ [ 0┃ ] ]"], "[[0")?;
        assert_insert_seq(&["┃"], &["[ [ \"abc┃\" ] ]"], "[[\"abc")?;
        assert_insert_seq(
            &["┃"],
            &["[ [ { camelCase: { a: 79000┃ } } ] ]"],
            "[[{camelCase:{a:79000",
        )?;

        Ok(())
    }

    #[test]
    fn test_ignore_single_elt_list() -> Result<(), String> {
        assert_insert_seq_ignore(&["┃[  ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[  ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[┃  ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[  ┃]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃[ 0 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ 0 ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[┃ 0 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ 0 ┃]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃[ 137 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ 137 ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[┃ 137 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ 137 ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ ┃137 ]"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore(&["[ 137┃ ]"], IGNORE_NO_NUM)?;

        assert_insert_seq_ignore(&["┃[ \"teststring\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ \"teststring\" ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[┃ \"teststring\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ \"teststring\" ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ ┃\"teststring\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ \"teststring\"┃ ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃[ { a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ { a: 1 } ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[┃ { a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ { a: 1 } ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ ┃{ a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ {┃ a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ { a:┃ 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ { a: 1 ┃} ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ { a: 1 }┃ ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃[ [  ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [  ] ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[┃ [  ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [  ] ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ ┃[  ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [  ]┃ ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [┃  ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [  ┃] ]"], IGNORE_CHARS)?;

        Ok(())
    }

    #[test]
    fn test_multi_elt_list() -> Result<(), String> {
        assert_insert_seq(&["┃"], &["[ 0, 1┃ ]"], "[0,1")?;
        assert_insert_seq(&["┃"], &["[ 987, 6543, 210┃ ]"], "[987,6543,210")?;

        assert_insert_seq(
            &["┃"],
            &["[ \"a\", \"bcd\", \"EFGH┃\" ]"],
            "[\"a🡲,\"bcd🡲,\"EFGH",
        )?;

        assert_insert_seq(
            &["┃"],
            &["[ { a: 1 }, { b: 23 }, { c: 456┃ } ]"],
            "[{a:1🡲🡲,{b:23🡲🡲,{c:456",
        )?;

        assert_insert_seq(&["┃"], &["[ [ 1 ], [ 23 ], [ 456┃ ] ]"], "[[1🡲🡲,[23🡲🡲,[456")?;

        // insert element in between
        assert_insert_seq(&["┃"], &["[ 0, 2┃, 1 ]"], "[0,1🡰🡰🡰,2")?;
        assert_insert_seq(&["┃"], &["[ 0, 2, 3┃, 1 ]"], "[0,1🡰🡰🡰,2,3")?;
        assert_insert_seq(&["┃"], &["[ 0, 3┃, 2, 1 ]"], "[0,1🡰🡰🡰,2🡰🡰🡰,3")?;

        assert_insert_seq(
            &["┃"],
            &["[ \"abc\", \"f┃\", \"de\" ]"],
            "[\"abc🡲,\"de🡰🡰🡰🡰🡰,\"f",
        )?;

        assert_insert_seq(&["┃"], &["[ [ 0 ], [ 2┃ ], [ 1 ] ]"], "[[0🡲🡲,[1🡰🡰🡰🡰🡰,[2")?;

        assert_insert_seq(
            &["┃"],
            &["[ { a: 0 }, { a: 2┃ }, { a: 1 } ]"],
            "[{a:0🡲🡲,{a:1🡰🡰🡰🡰🡰🡰🡰🡰,{a:2",
        )?;

        Ok(())
    }

    #[test]
    fn test_ignore_multi_elt_list() -> Result<(), String> {
        assert_insert_seq_ignore(&["┃[ 0, 1 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ 0, 1 ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[┃ 0, 1 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ 0, 1 ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ 0,┃ 1 ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃[ 123, 56, 7 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ 123, 56, 7 ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[┃ 123, 56, 7 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ 123, 56, 7 ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ 123,┃ 56, 7 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ 123, 56,┃ 7 ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃[ \"123\", \"56\", \"7\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ \"123\", \"56\", \"7\" ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[┃ \"123\", \"56\", \"7\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ \"123\", \"56\", \"7\" ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ \"123\",┃ \"56\", \"7\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ \"123\", \"56\",┃ \"7\" ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃[ { a: 0 }, { a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ { a: 0 }, { a: 1 } ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[┃ { a: 0 }, { a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ { a: 0 }, { a: 1 } ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ { a: 0 },┃ { a: 1 } ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore(&["┃[ [ 0 ], [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [ 0 ], [ 1 ] ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[┃ [ 0 ], [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [ 0 ], [ 1 ] ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [ 0 ],┃ [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ ┃[ 0 ], [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [ 0 ]┃, [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [┃ 0 ], [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [ 0 ┃], [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [ 0 ], ┃[ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [ 0 ], [┃ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [ 0 ], [ 1 ]┃ ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["[ [ 0 ], [ 1 ┃] ]"], IGNORE_CHARS)?;

        Ok(())
    }

    #[test]
    fn test_let_value() -> Result<(), String> {
        assert_insert(&["┃"], &["a┃ =  "], 'a')?;
        assert_insert(&["┃"], &["m┃ =  "], 'm')?;
        assert_insert(&["┃"], &["z┃ =  "], 'z')?;

        assert_insert_seq(&["┃"], &["ab┃ =  "], "ab")?;
        assert_insert_seq(&["┃"], &["mainVal┃ =  "], "mainVal")?;
        assert_insert_seq(&["┃"], &["camelCase123┃ =  "], "camelCase123")?;
        assert_insert_seq(&["┃"], &["c137┃ =  "], "c137")?;
        assert_insert_seq(&["┃"], &["c137Bb┃ =  "], "c137Bb")?;
        assert_insert_seq(&["┃"], &["bBbb┃ =  "], "bBbb")?;
        assert_insert_seq(&["┃"], &["cC0Z┃ =  "], "cC0Z")?;

        Ok(())
    }

    #[test]
    fn test_ignore_let_value() -> Result<(), String> {
        assert_insert_seq_ignore(&["a ┃= 0","a"], IGNORE_CHARS)?;
        assert_insert_seq_ignore(&["a =┃ 0", "a"], IGNORE_CHARS)?;

        Ok(())
    }

    // Create ed_model from pre_lines DSL, do ctrl+shift+up as many times as repeat.
    // check if modified ed_model has expected string representation of code, caret position and active selection.
    pub fn assert_ctrl_shift_up_repeat(
        pre_lines: &[&str],
        expected_post_lines: &[&str],
        repeats: usize,
    ) -> Result<(), String> {
        let test_arena = Bump::new();
        let code_str = BumpString::from_str_in(&pre_lines.join("").replace("┃", ""), &test_arena);

        let mut model_refs = init_model_refs();

        let mut ed_model = ed_model_from_dsl(&code_str, pre_lines, &mut model_refs)?;

        for _ in 0..repeats {
            ed_model.ed_handle_key_down(&ctrl_cmd_shift(), Up)?;
        }

        let post_lines = ui_res_to_res(ed_model_to_dsl(&ed_model))?;

        assert_eq!(post_lines, expected_post_lines);

        Ok(())
    }

    pub fn assert_ctrl_shift_up(
        pre_lines: &[&str],
        expected_post_lines: &[&str],
    ) -> Result<(), String> {
        assert_ctrl_shift_up_repeat(pre_lines, expected_post_lines, 1)
    }

    #[test]
    fn test_ctrl_shift_up_blank() -> Result<(), String> {
        // Blank is auto-inserted
        assert_ctrl_shift_up(&["┃"], &["┃❮ ❯"])?;
        assert_ctrl_shift_up_repeat(&["┃"], &["┃❮ ❯"], 4)?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_int() -> Result<(), String> {
        assert_ctrl_shift_up(&["5┃"], &["┃❮5❯"])?;
        assert_ctrl_shift_up_repeat(&["0┃"], &["┃❮0❯"], 3)?;
        assert_ctrl_shift_up(&["12345┃"], &["┃❮12345❯"])?;
        assert_ctrl_shift_up(&["┃12345"], &["┃❮12345❯"])?;
        assert_ctrl_shift_up(&["1┃2345"], &["┃❮12345❯"])?;
        assert_ctrl_shift_up(&["12┃345"], &["┃❮12345❯"])?;
        assert_ctrl_shift_up(&["123┃45"], &["┃❮12345❯"])?;
        assert_ctrl_shift_up(&["1234┃5"], &["┃❮12345❯"])?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_string() -> Result<(), String> {
        assert_ctrl_shift_up(&["\"┃\""], &["┃❮\"\"❯"])?;
        assert_ctrl_shift_up(&["┃\"\""], &["┃❮\"\"❯"])?;
        assert_ctrl_shift_up(&["\"┃0\""], &["┃❮\"0\"❯"])?;
        assert_ctrl_shift_up(&["\"0┃\""], &["┃❮\"0\"❯"])?;
        assert_ctrl_shift_up(&["\"abc┃\""], &["┃❮\"abc\"❯"])?;
        assert_ctrl_shift_up(&["\"ab┃c\""], &["┃❮\"abc\"❯"])?;
        assert_ctrl_shift_up(&["\"┃abc\""], &["┃❮\"abc\"❯"])?;
        assert_ctrl_shift_up(&["┃\"abc\""], &["┃❮\"abc\"❯"])?;
        assert_ctrl_shift_up_repeat(&["\"abc┃\""], &["┃❮\"abc\"❯"], 3)?;
        assert_ctrl_shift_up(
            &["\"hello, hello.0123456789ZXY{}[]-><-┃\""],
            &["┃❮\"hello, hello.0123456789ZXY{}[]-><-\"❯"],
        )?;

        assert_ctrl_shift_up(&["\"\"┃"], &["┃❮\"\"❯"])?;
        assert_ctrl_shift_up(&["\"abc\"┃"], &["┃❮\"abc\"❯"])?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_record() -> Result<(), String> {
        // TODO uncomment tests once editor::lang::constrain::constrain_expr does not contain anymore todo's
        assert_ctrl_shift_up(&["{ ┃ }"], &["┃❮{  }❯"])?;
        assert_ctrl_shift_up(&["{┃  }"], &["┃❮{  }❯"])?;
        assert_ctrl_shift_up(&["┃{  }"], &["┃❮{  }❯"])?;
        assert_ctrl_shift_up(&["{  ┃}"], &["┃❮{  }❯"])?;
        assert_ctrl_shift_up_repeat(&["{ ┃ }"], &["┃❮{  }❯"], 4)?;
        assert_ctrl_shift_up(&["{  }┃"], &["┃❮{  }❯"])?;

        /*assert_ctrl_shift_up(&["{ pear┃ }"], &["┃❮{ pear }❯"])?;
        assert_ctrl_shift_up(&["{ pea┃r }"], &["┃❮{ pear }❯"])?;
        assert_ctrl_shift_up(&["{ p┃ear }"], &["┃❮{ pear }❯"])?;
        assert_ctrl_shift_up(&["{ ┃pear }"], &["┃❮{ pear }❯"])?;
        assert_ctrl_shift_up(&["{┃ pear }"], &["┃❮{ pear }❯"])?;
        assert_ctrl_shift_up(&["┃{ pear }"], &["┃❮{ pear }❯"])?;
        assert_ctrl_shift_up(&["{ pear ┃}"], &["┃❮{ pear }❯"])?;
        assert_ctrl_shift_up_repeat(&["{ pear┃ }"], &["┃❮{ pear }❯"], 3)?;
        assert_ctrl_shift_up(&["{ pear }┃"], &["┃❮{ pear }❯"])?;

        assert_ctrl_shift_up(&["{ camelCase123┃ }"], &["┃❮{ camelCase123 }❯"])?;*/

        assert_ctrl_shift_up(&["{ a: \"┃\" }"], &["{ a: ┃❮\"\"❯ }"])?;
        assert_ctrl_shift_up(&["{ a: ┃\"\" }"], &["{ a: ┃❮\"\"❯ }"])?;
        assert_ctrl_shift_up(&["{ a: \"\"┃ }"], &["┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up(&["{ a: \"\" ┃}"], &["┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up_repeat(&["{ a: \"\" ┃}"], &["┃❮{ a: \"\" }❯"], 3)?;
        assert_ctrl_shift_up(&["{ a: \"\" }┃"], &["┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up(&["{ a:┃ \"\" }"], &["┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up(&["{ a┃: \"\" }"], &["┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up(&["{ ┃a: \"\" }"], &["┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up(&["{┃ a: \"\" }"], &["┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up(&["┃{ a: \"\" }"], &["┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up_repeat(&["{ a: \"┃\" }"], &["┃❮{ a: \"\" }❯"], 2)?;
        assert_ctrl_shift_up_repeat(&["{ a: \"┃\" }"], &["┃❮{ a: \"\" }❯"], 4)?;

        assert_ctrl_shift_up(&["{ a: 1┃0 }"], &["{ a: ┃❮10❯ }"])?;
        assert_ctrl_shift_up(&["{ a: ┃9 }"], &["{ a: ┃❮9❯ }"])?;
        assert_ctrl_shift_up(&["{ a: 98┃89 }"], &["{ a: ┃❮9889❯ }"])?;
        assert_ctrl_shift_up(&["{ a: 44┃ }"], &["┃❮{ a: 44 }❯"])?;
        assert_ctrl_shift_up(&["{ a: 0 ┃}"], &["┃❮{ a: 0 }❯"])?;
        assert_ctrl_shift_up_repeat(&["{ a: 123 ┃}"], &["┃❮{ a: 123 }❯"], 3)?;
        assert_ctrl_shift_up(&["{ a: 96 }┃"], &["┃❮{ a: 96 }❯"])?;
        assert_ctrl_shift_up(&["{ a:┃ 985600 }"], &["┃❮{ a: 985600 }❯"])?;
        assert_ctrl_shift_up(&["{ a┃: 5648 }"], &["┃❮{ a: 5648 }❯"])?;
        assert_ctrl_shift_up(&["{ ┃a: 1000000 }"], &["┃❮{ a: 1000000 }❯"])?;
        assert_ctrl_shift_up(&["{┃ a: 1 }"], &["┃❮{ a: 1 }❯"])?;
        assert_ctrl_shift_up(&["┃{ a: 900600 }"], &["┃❮{ a: 900600 }❯"])?;
        assert_ctrl_shift_up_repeat(&["{ a: 10┃000 }"], &["┃❮{ a: 10000 }❯"], 2)?;
        assert_ctrl_shift_up_repeat(&["{ a: ┃45 }"], &["┃❮{ a: 45 }❯"], 4)?;

        assert_ctrl_shift_up(&["{ abc: \"de┃\" }"], &["{ abc: ┃❮\"de\"❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: \"d┃e\" }"], &["{ abc: ┃❮\"de\"❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: \"┃de\" }"], &["{ abc: ┃❮\"de\"❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: ┃\"de\" }"], &["{ abc: ┃❮\"de\"❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: \"de\"┃ }"], &["┃❮{ abc: \"de\" }❯"])?;
        assert_ctrl_shift_up_repeat(&["{ abc: \"d┃e\" }"], &["┃❮{ abc: \"de\" }❯"], 2)?;
        assert_ctrl_shift_up_repeat(&["{ abc: \"d┃e\" }"], &["┃❮{ abc: \"de\" }❯"], 3)?;

        assert_ctrl_shift_up(
            &["{ camelCase123: \"hello, hello.012┃3456789ZXY{}[]-><-\" }"],
            &["{ camelCase123: ┃❮\"hello, hello.0123456789ZXY{}[]-><-\"❯ }"],
        )?;
        assert_ctrl_shift_up(
            &["{ camel┃Case123: \"hello, hello.0123456789ZXY{}[]-><-\" }"],
            &["┃❮{ camelCase123: \"hello, hello.0123456789ZXY{}[]-><-\" }❯"],
        )?;
        assert_ctrl_shift_up_repeat(
            &["{ camelCase123: \"hello, hello┃.0123456789ZXY{}[]-><-\" }"],
            &["┃❮{ camelCase123: \"hello, hello.0123456789ZXY{}[]-><-\" }❯"],
            2,
        )?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_nested_record() -> Result<(), String> {
        // TODO uncomment tests once editor::lang::constrain::constrain_expr does not contain anymore todo's
        assert_ctrl_shift_up(&["{ abc: { ┃ } }"], &["{ abc: ┃❮{  }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: {┃  } }"], &["{ abc: ┃❮{  }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: ┃{  } }"], &["{ abc: ┃❮{  }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: {  ┃} }"], &["{ abc: ┃❮{  }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: {  }┃ }"], &["┃❮{ abc: {  } }❯"])?;

        /*assert_ctrl_shift_up(&["{ abc: { ┃d } }"], &["{ abc: ┃❮{ d }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: {┃ d } }"], &["{ abc: ┃❮{ d }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: ┃{ d } }"], &["{ abc: ┃❮{ d }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: { d ┃} }"], &["{ abc: ┃❮{ d }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: { d┃e } }"], &["{ abc: ┃❮{ de }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: { d }┃ }"], &["┃❮{ abc: { d } }❯"])?;
        assert_ctrl_shift_up(&["┃{ abc: { d } }"], &["┃❮{ abc: { d } }❯"])?;*/

        assert_ctrl_shift_up(&["{ abc: { de: { ┃ } } }"], &["{ abc: { de: ┃❮{  }❯ } }"])?;
        assert_ctrl_shift_up(&["{ abc: { de: ┃{  } } }"], &["{ abc: { de: ┃❮{  }❯ } }"])?;
        assert_ctrl_shift_up(&["{ abc: { de: {  }┃ } }"], &["{ abc: ┃❮{ de: {  } }❯ }"])?;

        assert_ctrl_shift_up(&["{ abc: { de: \"┃\" } }"], &["{ abc: { de: ┃❮\"\"❯ } }"])?;
        assert_ctrl_shift_up(&["{ abc: { de: ┃\"\" } }"], &["{ abc: { de: ┃❮\"\"❯ } }"])?;
        assert_ctrl_shift_up(&["{ abc: { de: \"\"┃ } }"], &["{ abc: ┃❮{ de: \"\" }❯ }"])?;
        assert_ctrl_shift_up(
            &["{ abc: { de: \"f g┃\" } }"],
            &["{ abc: { de: ┃❮\"f g\"❯ } }"],
        )?;
        assert_ctrl_shift_up(
            &["{ abc: { de┃: \"f g\" } }"],
            &["{ abc: ┃❮{ de: \"f g\" }❯ }"],
        )?;
        assert_ctrl_shift_up(
            &["{ abc: {┃ de: \"f g\" } }"],
            &["{ abc: ┃❮{ de: \"f g\" }❯ }"],
        )?;
        assert_ctrl_shift_up(
            &["{ abc: { de: \"f g\" ┃} }"],
            &["{ abc: ┃❮{ de: \"f g\" }❯ }"],
        )?;
        assert_ctrl_shift_up(
            &["{ abc: { de: \"f g\" }┃ }"],
            &["┃❮{ abc: { de: \"f g\" } }❯"],
        )?;
        assert_ctrl_shift_up(
            &["┃{ abc: { de: \"f g\" } }"],
            &["┃❮{ abc: { de: \"f g\" } }❯"],
        )?;
        assert_ctrl_shift_up(
            &["{ abc: { de: \"f g\" } }┃"],
            &["┃❮{ abc: { de: \"f g\" } }❯"],
        )?;

        assert_ctrl_shift_up_repeat(
            &["{ abc: { de: \"f g┃\" } }"],
            &["{ abc: ┃❮{ de: \"f g\" }❯ }"],
            2,
        )?;
        assert_ctrl_shift_up_repeat(
            &["{ abc: { de: ┃\"f g\" } }"],
            &["┃❮{ abc: { de: \"f g\" } }❯"],
            3,
        )?;
        assert_ctrl_shift_up_repeat(
            &["{ abc: { de: ┃\"f g\" } }"],
            &["┃❮{ abc: { de: \"f g\" } }❯"],
            4,
        )?;

        assert_ctrl_shift_up(&["{ abc: { de: ┃951 } }"], &["{ abc: { de: ┃❮951❯ } }"])?;
        assert_ctrl_shift_up(&["{ abc: { de: 11┃0 } }"], &["{ abc: { de: ┃❮110❯ } }"])?;
        assert_ctrl_shift_up(&["{ abc: { de: 444┃ } }"], &["{ abc: ┃❮{ de: 444 }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: { de┃: 99 } }"], &["{ abc: ┃❮{ de: 99 }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: {┃ de: 0 } }"], &["{ abc: ┃❮{ de: 0 }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: { de: 230 ┃} }"], &["{ abc: ┃❮{ de: 230 }❯ }"])?;
        assert_ctrl_shift_up(&["{ abc: { de: 7 }┃ }"], &["┃❮{ abc: { de: 7 } }❯"])?;
        assert_ctrl_shift_up(&["┃{ abc: { de: 1 } }"], &["┃❮{ abc: { de: 1 } }❯"])?;
        assert_ctrl_shift_up(
            &["{ abc: { de: 111111 } }┃"],
            &["┃❮{ abc: { de: 111111 } }❯"],
        )?;

        assert_ctrl_shift_up_repeat(&["{ abc: { de: 1┃5 } }"], &["{ abc: ┃❮{ de: 15 }❯ }"], 2)?;
        assert_ctrl_shift_up_repeat(&["{ abc: { de: ┃55 } }"], &["┃❮{ abc: { de: 55 } }❯"], 3)?;
        assert_ctrl_shift_up_repeat(&["{ abc: { de: ┃400 } }"], &["┃❮{ abc: { de: 400 } }❯"], 4)?;

        /*assert_ctrl_shift_up_repeat(
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase┃ } } } } } } } }"],
            &["{ g: { oi: { ng: { d: ┃❮{ e: { e: { p: { camelCase } } } }❯ } } } }"],
            4,
        )?;
        assert_ctrl_shift_up_repeat(
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase┃ } } } } } } } }"],
            &["{ g: ┃❮{ oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } }❯ }"],
            7,
        )?;
        assert_ctrl_shift_up_repeat(
            &["{ g: { oi: { ng: { d: { e: { e: { p: { camelCase┃ } } } } } } } }"],
            &["┃❮{ g: { oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } } }❯"],
            9,
        )?;*/

        Ok(())
    }

    // Create ed_model from pre_lines DSL, do handle_new_char() with new_char_seq, select current Expr2,
    // check if generated tooltips match expected_tooltips.
    pub fn assert_type_tooltips_seq(
        pre_lines: &[&str],
        expected_tooltips: &[&str],
        new_char_seq: &str,
    ) -> Result<(), String> {
        let test_arena = Bump::new();
        let code_str = BumpString::from_str_in(&pre_lines.join("").replace("┃", ""), &test_arena);

        let mut model_refs = init_model_refs();

        let mut ed_model = ed_model_from_dsl(&code_str, pre_lines, &mut model_refs)?;

        for input_char in new_char_seq.chars() {
            if input_char == '🡲' {
                ed_model.simple_move_carets_right(1);
            } else {
                ed_res_to_res(handle_new_char(&input_char, &mut ed_model))?;
            }
        }

        for expected_tooltip in expected_tooltips.iter() {
            ed_model.select_expr()?;

            let created_tooltip = ed_model.selected_expr_opt.unwrap().type_str;

            assert_eq!(
                created_tooltip.as_str(ed_model.module.env.pool),
                *expected_tooltip
            );
        }

        Ok(())
    }

    // Create ed_model from pre_lines DSL, do handle_new_char() with new_char, select current Expr2,
    // check if generated tooltip matches expected_tooltip.
    pub fn assert_type_tooltip(
        pre_lines: &[&str],
        expected_tooltip: &str,
        new_char: char,
    ) -> Result<(), String> {
        assert_type_tooltips_seq(pre_lines, &vec![expected_tooltip], &new_char.to_string())
    }

    pub fn assert_type_tooltip_clean(lines: &[&str], expected_tooltip: &str) -> Result<(), String> {
        assert_type_tooltips_seq(lines, &vec![expected_tooltip], "")
    }

    // When doing ctrl+shift+up multiple times we select the surrounding expression every time,
    // every new selection should have the correct tooltip
    pub fn assert_type_tooltips_clean(
        lines: &[&str],
        expected_tooltips: &[&str],
    ) -> Result<(), String> {
        assert_type_tooltips_seq(lines, expected_tooltips, "")
    }

    #[test]
    fn test_type_tooltip() -> Result<(), String> {
        assert_type_tooltip(&["┃"], "{}", '{')?;

        assert_type_tooltip_clean(&["┃5"], "Num *")?;
        assert_type_tooltip_clean(&["42┃"], "Num *")?;
        assert_type_tooltip_clean(&["13┃7"], "Num *")?;

        assert_type_tooltip_clean(&["\"┃abc\""], "Str")?;
        assert_type_tooltip_clean(&["┃\"abc\""], "Str")?;
        assert_type_tooltip_clean(&["\"abc\"┃"], "Str")?;

        assert_type_tooltip_clean(&["{ a: \"abc\" }┃"], "{ a : Str }")?;
        assert_type_tooltip_clean(&["{ ┃a: 0 }"], "{ a : Num * }")?;
        assert_type_tooltip_clean(&["{ ┃z: {  } }"], "{ z : {} }")?;
        assert_type_tooltip_clean(&["{ camelCase: ┃0 }"], "Num *")?;

        assert_type_tooltips_seq(&["┃"], &["*"], "")?;
        assert_type_tooltips_seq(&["┃"], &["*", "{ a : * }"], "{a:")?;

        assert_type_tooltips_clean(&["{ camelCase: ┃0 }"], &["Num *", "{ camelCase : Num * }"])?;
        assert_type_tooltips_clean(
            &["{ a: { b: { c: \"hello┃, hello.0123456789ZXY{}[]-><-\" } } }"],
            &[
                "Str",
                "{ c : Str }",
                "{ b : { c : Str } }",
                "{ a : { b : { c : Str } } }",
            ],
        )?;

        Ok(())
    }

    #[test]
    fn test_type_tooltip_list() -> Result<(), String> {
        assert_type_tooltip(&["┃"], "List *", '[')?;
        assert_type_tooltips_seq(&["┃"], &["List (Num *)"], "[0")?;
        assert_type_tooltips_seq(&["┃"], &["List (Num *)", "List (List (Num *))"], "[[0")?;
        assert_type_tooltips_seq(&["┃"], &["Str", "List Str"], "[\"a")?;
        assert_type_tooltips_seq(
            &["┃"],
            &[
                "Str",
                "List Str",
                "List (List Str)",
                "List (List (List Str))",
            ],
            "[[[\"a",
        )?;
        assert_type_tooltips_seq(
            &["┃"],
            &[
                "{ a : Num * }",
                "List { a : Num * }",
                "List (List { a : Num * })",
            ],
            "[[{a:1",
        )?;

        // multi element lists
        assert_type_tooltips_seq(&["┃"], &["List (Num *)"], "[1,2,3")?;
        assert_type_tooltips_seq(&["┃"], &["Str", "List Str"], "[\"abc🡲,\"de🡲,\"f")?;
        assert_type_tooltips_seq(
            &["┃"],
            &["{ a : Num * }", "List { a : Num * }"],
            "[{a:0🡲🡲,{a:12🡲🡲,{a:444",
        )?;
        Ok(())
    }

    #[test]
    fn test_type_tooltip_mismatch() -> Result<(), String> {
        assert_type_tooltips_seq(&["┃"], &["Str", "List <type mismatch>"], "[1,\"abc")?;
        assert_type_tooltips_seq(&["┃"], &["List <type mismatch>"], "[\"abc🡲,50")?;

        assert_type_tooltips_seq(
            &["┃"],
            &["Str", "{ a : Str }", "List <type mismatch>"],
            "[{a:0🡲🡲,{a:\"0",
        )?;

        assert_type_tooltips_seq(
            &["┃"],
            &["List (Num *)", "List (List <type mismatch>)"],
            "[[0,1,\"2🡲🡲🡲,[3, 4, 5",
        )?;

        Ok(())
    }

    type ModelMoveCaretFun = fn(&mut EdModel<'_>, &Modifiers) -> UIResult<()>;

    // Create ed_model from pre_lines DSL, do ctrl+shift+up as many times as repeat. Then move the caret by executing
    // move_caret_fun. Next check if modified ed_model has expected string representation of code, caret position and
    // active selection.
    fn assert_ctrl_shift_up_move(
        pre_lines: &[&str],
        expected_post_lines: &[&str],
        repeats: usize,
        move_caret_fun: ModelMoveCaretFun,
    ) -> Result<(), String> {
        let test_arena = Bump::new();
        let code_str = BumpString::from_str_in(&pre_lines.join("").replace("┃", ""), &test_arena);

        let mut model_refs = init_model_refs();

        let mut ed_model = ed_model_from_dsl(&code_str, pre_lines, &mut model_refs)?;

        for _ in 0..repeats {
            ed_model.ed_handle_key_down(&ctrl_cmd_shift(), Up)?;
        }

        move_caret_fun(&mut ed_model, &no_mods())?;

        let post_lines = ui_res_to_res(ed_model_to_dsl(&ed_model))?;

        assert_eq!(post_lines, expected_post_lines);

        Ok(())
    }

    fn assert_ctrl_shift_single_up_move(
        pre_lines: &[&str],
        expected_post_lines: &[&str],
        move_caret_fun: ModelMoveCaretFun,
    ) -> Result<(), String> {
        assert_ctrl_shift_up_move(pre_lines, expected_post_lines, 1, move_caret_fun)
    }

    // because complex lifetime stuff
    macro_rules! move_right {
        () => {
            |ed_model, modifiers| EdModel::move_caret_right(ed_model, modifiers)
        };
    }
    macro_rules! move_left {
        () => {
            |ed_model, modifiers| EdModel::move_caret_left(ed_model, modifiers)
        };
    }
    macro_rules! move_up {
        () => {
            |ed_model, modifiers| EdModel::move_caret_up(ed_model, modifiers)
        };
    }
    macro_rules! move_down {
        () => {
            |ed_model, modifiers| EdModel::move_caret_down(ed_model, modifiers)
        };
    }
    macro_rules! move_home {
        () => {
            |ed_model, modifiers| EdModel::move_caret_home(ed_model, modifiers)
        };
    }
    macro_rules! move_end {
        () => {
            |ed_model, modifiers| EdModel::move_caret_end(ed_model, modifiers)
        };
    }

    #[test]
    fn test_ctrl_shift_up_move_blank() -> Result<(), String> {
        // Blank is auto-inserted
        assert_ctrl_shift_single_up_move(&["┃"], &[" ┃"], move_right!())?;
        assert_ctrl_shift_up_move(&["┃"], &["┃ "], 3, move_left!())?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_move_int() -> Result<(), String> {
        assert_ctrl_shift_single_up_move(&["┃0"], &["0┃"], move_down!())?;
        assert_ctrl_shift_single_up_move(&["┃9654"], &["┃9654"], move_up!())?;
        assert_ctrl_shift_single_up_move(&["┃100546"], &["100546┃"], move_end!())?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_move_string() -> Result<(), String> {
        assert_ctrl_shift_single_up_move(&["┃\"\""], &["\"\"┃"], move_down!())?;
        assert_ctrl_shift_single_up_move(&["┃\"abc\""], &["┃\"abc\""], move_up!())?;
        assert_ctrl_shift_single_up_move(
            &["┃\"hello, hello.0123456789ZXY{}[]-><-\""],
            &["\"hello, hello.0123456789ZXY{}[]-><-\"┃"],
            move_end!(),
        )?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_move_record() -> Result<(), String> {
        // TODO uncomment tests once editor::lang::constrain::constrain_expr does not contain anymore todo's
        assert_ctrl_shift_single_up_move(&["┃{  }"], &["┃{  }"], move_home!())?;
        //assert_ctrl_shift_single_up_move(&["┃{ a }"], &["{ a }┃"], move_down!())?;
        //assert_ctrl_shift_single_up_move(&["┃{ a: { b } }"], &["{ a: { b } }┃"], move_right!())?;
        assert_ctrl_shift_single_up_move(&["{ a: { ┃ } }"], &["{ a: {  } }┃"], move_end!())?;
        assert_ctrl_shift_up_move(
            &["{ a: { b: { ┃ } } }"],
            &["{ a: ┃{ b: {  } } }"],
            2,
            move_up!(),
        )?;
        assert_ctrl_shift_up_move(
            &["{ camelCase: { cC123: \"hello┃, hello.0123456789ZXY{}[]-><-\" } }"],
            &["{ camelCase: { cC123: \"hello, hello.0123456789ZXY{}[]-><-\" }┃ }"],
            2,
            move_down!(),
        )?;

        assert_ctrl_shift_up_move(
            &["{ camelCase: { cC123: 9┃5 } }"],
            &["{ camelCase: { cC123: 95 }┃ }"],
            2,
            move_down!(),
        )?;

        Ok(())
    }

    // Create ed_model from pre_lines DSL, do ctrl+shift+up as many times as repeat. Then do backspace.
    // Next check if modified ed_model has expected string representation of code, caret position and
    // active selection.
    fn assert_ctrl_shift_up_backspace(
        pre_lines: &[&str],
        expected_post_lines: &[&str],
        repeats: usize,
    ) -> Result<(), String> {
        let test_arena = Bump::new();
        let code_str = BumpString::from_str_in(&pre_lines.join("").replace("┃", ""), &test_arena);

        let mut model_refs = init_model_refs();

        let mut ed_model = ed_model_from_dsl(&code_str, pre_lines, &mut model_refs)?;

        for _ in 0..repeats {
            ed_model.ed_handle_key_down(&ctrl_cmd_shift(), Up)?;
        }

        handle_new_char(&'\u{8}', &mut ed_model)?; // \u{8} is the char for backspace on linux

        let post_lines = ui_res_to_res(ed_model_to_dsl(&ed_model))?;

        assert_eq!(post_lines, expected_post_lines);

        Ok(())
    }

    fn assert_ctrl_shift_single_up_backspace(
        pre_lines: &[&str],
        expected_post_lines: &[&str],
    ) -> Result<(), String> {
        assert_ctrl_shift_up_backspace(pre_lines, expected_post_lines, 1)
    }

    #[test]
    fn test_ctrl_shift_up_backspace_blank() -> Result<(), String> {
        // Blank is inserted when root is deleted
        assert_ctrl_shift_single_up_backspace(&["┃"], &["┃ "])?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_backspace_int() -> Result<(), String> {
        // Blank is inserted when root is deleted
        assert_ctrl_shift_single_up_backspace(&["95┃21"], &["┃ "])?;
        assert_ctrl_shift_single_up_backspace(&["0┃"], &["┃ "])?;
        assert_ctrl_shift_single_up_backspace(&["┃10000"], &["┃ "])?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_backspace_string() -> Result<(), String> {
        // Blank is inserted when root is deleted
        assert_ctrl_shift_single_up_backspace(&["\"┃\""], &["┃ "])?;
        assert_ctrl_shift_single_up_backspace(&["\"\"┃"], &["┃ "])?;
        assert_ctrl_shift_single_up_backspace(&["┃\"abc\""], &["┃ "])?;
        assert_ctrl_shift_single_up_backspace(
            &["\"hello┃, hello.0123456789ZXY{}[]-><-\""],
            &["┃ "],
        )?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_backspace_record() -> Result<(), String> {
        // TODO uncomment tests once editor::lang::constrain::constrain_expr does not contain anymore todo's
        // Blank is inserted when root is deleted
        assert_ctrl_shift_single_up_backspace(&["{┃  }"], &["┃ "])?;
        //assert_ctrl_shift_single_up_backspace(&["{ a┃ }"], &["┃ "])?;
        //assert_ctrl_shift_single_up_backspace(&["{ a: { b }┃ }"], &["┃ "])?;
        assert_ctrl_shift_single_up_backspace(&["{ a: \"b cd\"┃ }"], &["┃ "])?;

        //assert_ctrl_shift_single_up_backspace(&["{ a: ┃{ b } }"], &["{ a: ┃  }"])?;
        assert_ctrl_shift_single_up_backspace(&["{ a: \"┃b cd\" }"], &["{ a: ┃  }"])?;
        assert_ctrl_shift_single_up_backspace(&["{ a: ┃12 }"], &["{ a: ┃  }"])?;
        /*assert_ctrl_shift_single_up_backspace(
            &["{ g: { oi: { ng: { d: { ┃e: { e: { p: { camelCase } } } } } } } }"],
            &["{ g: { oi: { ng: { d: ┃  } } } }"],
        )?;*/

        assert_ctrl_shift_up_backspace(
            &["{ a: { b: { c: \"abc┃  \" } } }"],
            &["{ a: { b: ┃  } }"],
            2,
        )?;
        assert_ctrl_shift_up_backspace(
            &["{ a: { b: { c: 100┃000 } } }"],
            &["{ a: { b: ┃  } }"],
            2,
        )?;
        assert_ctrl_shift_up_backspace(&["{ a: { b: { c: {┃  } } } }"], &["{ a: { b: ┃  } }"], 2)?;
        /*assert_ctrl_shift_up_backspace(
            &["{ g: { oi: { ng: { d: { e: { e: { p┃: { camelCase } } } } } } } }"],
            &["{ g: ┃  }"],
            6,
        )?;*/

        Ok(())
    }
}
