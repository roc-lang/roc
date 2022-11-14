#![allow(dead_code)]

use std::process::Stdio;

use crate::editor::code_lines::CodeLines;
use crate::editor::ed_error::EdResult;
use crate::editor::ed_error::{MissingSelectionSnafu, RocCheckFailedSnafu};
use crate::editor::grid_node_map::GridNodeMap;
use crate::editor::mvc::app_update::InputOutcome;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::mvc::ed_model::SelectedBlock;
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
use crate::editor::mvc::tld_value_update::{start_new_tld_value, update_tld_val_name};
#[cfg(feature = "with_sound")]
use crate::editor::sound::play_sound;
use crate::ui::text::caret_w_select::CaretWSelect;
use crate::ui::text::lines::MoveCaretFun;
use crate::ui::text::selection::validate_raw_sel;
use crate::ui::text::selection::RawSelection;
use crate::ui::text::selection::Selection;
use crate::ui::text::text_pos::TextPos;
use crate::ui::text::{lines, lines::Lines, lines::SelectableLines};
use crate::ui::ui_error::UIResult;
use crate::ui::util::path_to_string;
use crate::ui::util::write_to_file;
use crate::window::keyboard_input::Modifiers;
use bumpalo::Bump;
use roc_ast::constrain::constrain_expr;
use roc_ast::constrain::Constraint;
use roc_ast::lang::core::ast::ASTNodeId;
use roc_ast::lang::core::def::def2::Def2;
use roc_ast::lang::core::def::def2::DefId;
use roc_ast::lang::core::expr::expr2::Expr2;
use roc_ast::lang::core::expr::expr2::ExprId;
use roc_ast::lang::core::types::Type2;
use roc_ast::mem_pool::pool::Pool;
use roc_ast::mem_pool::pool_str::PoolStr;
use roc_ast::solve_type;
use roc_can::expected::Expected;
use roc_code_markup::markup::attribute::Attributes;
use roc_code_markup::markup::convert::from_ast::ast_to_mark_nodes;
use roc_code_markup::markup::nodes;
use roc_code_markup::markup::nodes::MarkupNode;
use roc_code_markup::markup::nodes::EQUALS;
use roc_code_markup::slow_pool::MarkNodeId;
use roc_code_markup::slow_pool::SlowPool;
use roc_collections::all::MutMap;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use roc_solve::module::Solved;
use roc_types::pretty_print::name_and_print_var;
use roc_types::pretty_print::DebugPrint;
use roc_types::subs::{Subs, VarStore, Variable};
use roc_utils::cargo;
use snafu::OptionExt;
use threadpool::ThreadPool;
use winit::event::VirtualKeyCode;
use VirtualKeyCode::*;

use super::break_line::break_line;
use super::break_line::insert_new_blank;
use super::let_update::start_new_let_value;

/// ed_update.rs contains all functions that change the ed_model.
/// Additions and deletions of new characters to the editor are handled here.
/// A large percentage of the editor's tests are at the end of this file.
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
        self.selected_block_opt = None;

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
    // TODO error if no match was found for old_caret_pos
    pub fn simple_move_caret_right(&mut self, old_caret_pos: TextPos, repeat: usize) {
        for caret_tup in self.caret_w_select_vec.iter_mut() {
            if caret_tup.0.caret_pos == old_caret_pos {
                caret_tup.0.caret_pos.column += repeat;
                caret_tup.1 = None;
            }
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

    // disregards EdModel.code_lines because the caller knows the resulting caret position will be valid.
    // allows us to prevent multiple updates to EdModel.code_lines
    pub fn simple_move_carets_down(&mut self, repeat: usize) {
        for caret_tup in self.caret_w_select_vec.iter_mut() {
            caret_tup.0.caret_pos.column = 0;
            caret_tup.0.caret_pos.line += repeat;
            caret_tup.1 = None;
        }
    }

    // disregards EdModel.code_lines because the caller knows the resulting caret position will be valid.
    // allows us to prevent multiple updates to EdModel.code_lines
    // TODO error if no match was found for old_caret_pos
    pub fn simple_move_caret_down(&mut self, old_caret_pos: TextPos, repeat: usize) {
        for caret_tup in self.caret_w_select_vec.iter_mut() {
            if caret_tup.0.caret_pos == old_caret_pos {
                caret_tup.0.caret_pos.column = 0;
                caret_tup.0.caret_pos.line += repeat;
                caret_tup.1 = None;
            }
        }
    }

    // disregards EdModel.code_lines because the caller knows the resulting caret position will be valid.
    // allows us to prevent multiple updates to EdModel.code_lines
    pub fn simple_move_carets_up(&mut self, repeat: usize) {
        for caret_tup in self.caret_w_select_vec.iter_mut() {
            caret_tup.0.caret_pos.line -= repeat;
            caret_tup.1 = None;
        }
    }

    pub fn add_mark_node(&mut self, node: MarkupNode) -> MarkNodeId {
        self.mark_node_pool.add(node)
    }

    fn build_markup_string(
        node_id: MarkNodeId,
        all_code_string: &mut String,
        mark_node_pool: &SlowPool,
    ) -> EdResult<()> {
        let node = mark_node_pool.get(node_id);

        if node.is_nested() {
            for child_id in node.get_children_ids() {
                EdModel::build_markup_string(child_id, all_code_string, mark_node_pool)?;
            }
        } else {
            let node_content_str = node.get_content();

            all_code_string.push_str(&node_content_str);
        }

        for _ in 0..node.get_newlines_at_end() {
            all_code_string.push('\n');
        }

        Ok(())
    }

    // updates grid_node_map and code_lines but nothing else.
    pub fn insert_between_line(
        line_nr: usize,
        index: usize,
        new_str: &str,
        node_id: MarkNodeId,
        grid_node_map: &mut GridNodeMap,
    ) -> UIResult<()> {
        grid_node_map.insert_between_line(line_nr, index, new_str.len(), node_id)
    }

    pub fn insert_all_between_line(
        &mut self,
        line_nr: usize,
        index: usize,
        leaf_node_ids: &[MarkNodeId],
    ) -> UIResult<()> {
        let mut col_nr = index;
        let mut curr_line_nr = line_nr;

        for &node_id in leaf_node_ids {
            let mark_node = self.mark_node_pool.get(node_id);
            let node_full_content = mark_node.get_full_content();

            if node_full_content.contains('\n') {
                //insert separate lines separately
                let split_lines = node_full_content.split('\n');

                for line in split_lines {
                    self.grid_node_map.insert_between_line(
                        curr_line_nr,
                        col_nr,
                        line.len(),
                        node_id,
                    )?;

                    curr_line_nr += 1;
                    col_nr = 0;
                }
            } else {
                let node_content = mark_node.get_content();

                self.grid_node_map.insert_between_line(
                    line_nr,
                    col_nr,
                    node_content.len(),
                    node_id,
                )?;

                col_nr += node_content.len();
            }
        }

        Ok(())
    }

    pub fn insert_mark_node_between_line(
        line_nr: &mut usize,
        col_nr: &mut usize,
        mark_node_id: MarkNodeId,
        grid_node_map: &mut GridNodeMap,
        mark_node_pool: &SlowPool,
    ) -> UIResult<()> {
        let mark_node = mark_node_pool.get(mark_node_id);

        let node_newlines = mark_node.get_newlines_at_end();

        if mark_node.is_nested() {
            let children_ids = mark_node.get_children_ids();

            for child_id in children_ids {
                EdModel::insert_mark_node_between_line(
                    line_nr,
                    col_nr,
                    child_id,
                    grid_node_map,
                    mark_node_pool,
                )?;
            }
        } else {
            let node_content = mark_node.get_content();

            EdModel::insert_between_line(
                *line_nr,
                *col_nr,
                &node_content,
                mark_node_id,
                grid_node_map,
            )?;

            *col_nr += node_content.len();
        }

        if node_newlines > 0 {
            EdModel::break_line(*line_nr, *col_nr, grid_node_map)?;

            *line_nr += 1;
            *col_nr = 0;

            for _ in 1..node_newlines {
                EdModel::insert_empty_line(*line_nr, grid_node_map)?;

                *line_nr += 1;
                *col_nr = 0;
            }
        }

        Ok(())
    }

    // break(split) line at col_nr and move everything after col_nr to the next line
    pub fn break_line(
        line_nr: usize,
        col_nr: usize,
        grid_node_map: &mut GridNodeMap,
    ) -> UIResult<()> {
        grid_node_map.break_line(line_nr, col_nr)
    }

    pub fn insert_empty_line(line_nr: usize, grid_node_map: &mut GridNodeMap) -> UIResult<()> {
        grid_node_map.insert_empty_line(line_nr)
    }

    pub fn push_empty_line(grid_node_map: &mut GridNodeMap) {
        grid_node_map.push_empty_line();
    }

    pub fn clear_line(&mut self, line_nr: usize) -> UIResult<()> {
        self.grid_node_map.clear_line(line_nr)
    }

    pub fn del_line(&mut self, line_nr: usize) {
        self.grid_node_map.del_line(line_nr)
    }

    pub fn del_at_line(&mut self, line_nr: usize, index: usize) -> UIResult<()> {
        self.grid_node_map.del_at_line(line_nr, index)
    }

    // updates grid_node_map and code_lines but nothing else.
    pub fn del_range_at_line(
        &mut self,
        line_nr: usize,
        col_range: std::ops::Range<usize>,
    ) -> UIResult<()> {
        self.grid_node_map.del_range_at_line(line_nr, col_range)
    }

    pub fn del_blank_expr_node(&mut self, txt_pos: TextPos) -> UIResult<()> {
        self.del_at_line(txt_pos.line, txt_pos.column)
    }

    pub fn set_selected_expr(
        &mut self,
        expr_start_pos: TextPos,
        expr_end_pos: TextPos,
        ast_node_id: ASTNodeId,
        mark_node_id: MarkNodeId,
    ) -> EdResult<()> {
        self.set_raw_sel(RawSelection {
            start_pos: expr_start_pos,
            end_pos: expr_end_pos,
        })?;

        self.set_caret(expr_start_pos);

        let type_str = match ast_node_id {
            ASTNodeId::ADefId(def_id) => {
                if let Some(expr_id) = self.extract_expr_from_def(def_id) {
                    self.expr2_to_type(expr_id)
                } else {
                    PoolStr::new(" * ", self.module.env.pool)
                }
            }

            ASTNodeId::AExprId(expr_id) => self.expr2_to_type(expr_id),
        };

        self.selected_block_opt = Some(SelectedBlock {
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
        if let Some(selected_block) = &self.selected_block_opt {
            let expr2_level_mark_node = self.mark_node_pool.get(selected_block.mark_node_id);

            if let Some(parent_id) = expr2_level_mark_node.get_parent_id_opt() {
                let ast_node_id = self.mark_id_ast_id_map.get(parent_id)?;

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
                    .get_block_start_end_pos(self.get_caret(), self)?;

                self.set_selected_expr(expr_start_pos, expr_end_pos, ast_node_id, mark_node_id)?;
            } else if self
                .grid_node_map
                .node_exists_at_pos(caret_pos.decrement_col())
            {
                let (expr_start_pos, expr_end_pos, ast_node_id, mark_node_id) = self
                    .grid_node_map
                    .get_block_start_end_pos(self.get_caret().decrement_col(), self)?;

                self.set_selected_expr(expr_start_pos, expr_end_pos, ast_node_id, mark_node_id)?;
            }
        }

        Ok(())
    }

    fn extract_expr_from_def(&self, def_id: DefId) -> Option<ExprId> {
        let def = self.module.env.pool.get(def_id);

        match def {
            Def2::ValueDef {
                identifier_id: _,
                expr_id,
            } => Some(*expr_id),
            Def2::Blank => None,
            Def2::CommentsBefore {
                comments: _,
                def_id,
            } => self.extract_expr_from_def(*def_id),
            Def2::CommentsAfter {
                comments: _,
                def_id,
            } => self.extract_expr_from_def(*def_id),
        }
    }

    fn expr2_to_type(&mut self, expr2_id: ExprId) -> PoolStr {
        let var = self.module.env.var_store.fresh();
        let expr = self.module.env.pool.get(expr2_id);
        let arena = Bump::new();

        let constrained = constrain_expr(
            &arena,
            &mut self.module.env,
            expr,
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

        let pretty_var = name_and_print_var(
            var,
            subs,
            self.module.env.home,
            &self.loaded_module.interns,
            DebugPrint::NOTHING,
        );

        PoolStr::new(&pretty_var, self.module.env.pool)
    }

    fn run_solve(
        mempool: &mut Pool,
        aliases: MutMap<Symbol, roc_types::types::Alias>,
        rigid_variables: MutMap<Variable, Lowercase>,
        constraint: Constraint,
        var_store: VarStore,
    ) -> (Solved<Subs>, solve_type::Env, Vec<solve_type::TypeError>) {
        let env = solve_type::Env {
            vars_by_symbol: MutMap::default(),
            aliases,
        };
        let arena = Bump::new();

        let mut subs = Subs::new_from_varstore(var_store);

        for (var, name) in rigid_variables {
            subs.rigid_var(var, name);
        }

        // Now that the module is parsed, canonicalized, and constrained,
        // we need to type check it.
        let mut problems = Vec::new();

        // Run the solver to populate Subs.
        let (solved_subs, solved_env) =
            solve_type::run(&arena, mempool, &env, &mut problems, subs, &constraint);

        (solved_subs, solved_env, problems)
    }

    pub fn ed_handle_key_down(
        &mut self,
        modifiers: &Modifiers,
        virtual_keycode: VirtualKeyCode,
        _sound_thread_pool: &mut ThreadPool,
    ) -> EdResult<()> {
        match virtual_keycode {
            Left => self.move_caret_left(modifiers)?,
            Up => {
                if modifiers.cmd_or_ctrl() && modifiers.shift {
                    self.select_expr()?
                } else {
                    self.move_caret_up(modifiers)?
                }
            }
            Right => self.move_caret_right(modifiers)?,
            Down => self.move_caret_down(modifiers)?,

            A => {
                if modifiers.cmd_or_ctrl() {
                    self.select_all()?
                }
            }
            S => {
                if modifiers.cmd_or_ctrl() {
                    self.save_file()?
                }
            }
            R => {
                if modifiers.cmd_or_ctrl() {
                    self.check_file()?;
                    self.run_file()?
                }
            }

            Home => self.move_caret_home(modifiers)?,
            End => self.move_caret_end(modifiers)?,

            F11 => {
                self.show_debug_view = !self.show_debug_view;
                self.dirty = true;
            }
            F12 => {
                #[cfg(feature = "with_sound")]
                _sound_thread_pool.execute(move || {
                    play_sound("./editor/src/editor/resources/sounds/bell_sound.mp3");
                });
            }
            _ => (),
        }

        Ok(())
    }

    // Replaces selected expression with blank.
    // If no expression is selected, this function will select one to guide the user to using backspace in a projectional editing way
    fn backspace(&mut self) -> EdResult<()> {
        if let Some(sel_block) = &self.selected_block_opt {
            let expr2_level_mark_node = self.mark_node_pool.get(sel_block.mark_node_id);
            let newlines_at_end = expr2_level_mark_node.get_newlines_at_end();

            let blank_replacement = MarkupNode::Blank {
                attributes: Attributes::default(),
                parent_id_opt: expr2_level_mark_node.get_parent_id_opt(),
                newlines_at_end,
            };

            self.mark_node_pool
                .replace_node(sel_block.mark_node_id, blank_replacement);

            let active_selection = self.get_selection().context(MissingSelectionSnafu {})?;

            self.grid_node_map.del_selection(active_selection)?;

            match sel_block.ast_node_id {
                ASTNodeId::ADefId(def_id) => {
                    self.module.env.pool.set(def_id, Def2::Blank);
                }
                ASTNodeId::AExprId(expr_id) => {
                    self.module.env.pool.set(expr_id, Expr2::Blank);
                }
            }

            let expr_mark_node_id = sel_block.mark_node_id;

            let caret_pos = self.get_caret();

            EdModel::insert_between_line(
                caret_pos.line,
                caret_pos.column,
                nodes::BLANK_PLACEHOLDER,
                expr_mark_node_id,
                &mut self.grid_node_map,
            )?;

            self.set_sel_none();
        } else {
            self.select_expr()?;
        };

        Ok(())
    }

    fn save_file(&mut self) -> UIResult<()> {
        let all_lines_str = self.code_lines.all_lines_as_string();

        write_to_file(self.file_path, &all_lines_str)?;

        println!("\nsave successful!");

        Ok(())
    }

    fn check_file(&mut self) -> EdResult<()> {
        println!("\nChecking file (cargo run check <file>)...");

        let roc_file_str = path_to_string(self.file_path);

        let cmd_out = cargo()
            .arg("run")
            .arg("--release")
            .arg("check")
            .arg(roc_file_str)
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .output()?;

        if !cmd_out.status.success() {
            RocCheckFailedSnafu.fail()?
        }

        Ok(())
    }

    fn run_file(&mut self) -> EdResult<()> {
        println!("\nExecuting file (cargo run --release <file>)...");

        let roc_file_str = path_to_string(self.file_path);

        cargo()
            .arg("run")
            .arg("--release")
            .arg(roc_file_str)
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .output()?;

        Ok(())
    }

    /// update MarkupNode's, grid_node_map, code_lines after the AST has been updated
    fn post_process_ast_update(&mut self) -> EdResult<()> {
        //dbg!("{}",self.module.ast.ast_to_string(self.module.env.pool));

        let markup_ids_tup = ast_to_mark_nodes(
            &mut self.module.env,
            &self.module.ast,
            &mut self.mark_node_pool,
            &self.loaded_module.interns,
        )?;

        self.markup_ids = markup_ids_tup.0;
        self.mark_id_ast_id_map = markup_ids_tup.1;

        self.code_lines = CodeLines::from_str(&nodes::mark_nodes_to_string(
            &self.markup_ids,
            &self.mark_node_pool,
        ));
        self.grid_node_map = GridNodeMap::default();

        let mut line_nr = 0;
        let mut col_nr = 0;

        for mark_node_id in &self.markup_ids {
            // for debugging:
            //println!("{}", tree_as_string(*mark_node_id, &mark_node_pool));
            EdModel::insert_mark_node_between_line(
                &mut line_nr,
                &mut col_nr,
                *mark_node_id,
                &mut self.grid_node_map,
                &self.mark_node_pool,
            )?
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
                let line_ref = self.code_lines.get_line_ref(start_line_index)?;

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
        self.selected_block_opt = None;
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
        let last_line = self.code_lines.get_line_ref(last_line_index)?;

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
    pub ast_node_id: ASTNodeId,
}

pub fn get_node_context<'a>(ed_model: &'a EdModel) -> EdResult<NodeContext<'a>> {
    let old_caret_pos = ed_model.get_caret();
    let curr_mark_node_id = ed_model
        .grid_node_map
        .get_id_at_row_col(ed_model.get_caret())?;
    let curr_mark_node = ed_model.mark_node_pool.get(curr_mark_node_id);
    let parent_id_opt = curr_mark_node.get_parent_id_opt();
    let ast_node_id = ed_model.mark_id_ast_id_map.get(curr_mark_node_id)?;

    Ok(NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt,
        ast_node_id,
    })
}

fn if_modifiers(modifiers: &Modifiers, shortcut_result: UIResult<()>) -> EdResult<()> {
    if modifiers.cmd_or_ctrl() {
        Ok(shortcut_result?)
    } else {
        Ok(())
    }
}

// handle new char when current(=caret is here) MarkupNode corresponds to a Def2 in the AST
pub fn handle_new_char_def(
    received_char: &char,
    def_id: DefId,
    ed_model: &mut EdModel,
) -> EdResult<InputOutcome> {
    let def_ref = ed_model.module.env.pool.get(def_id);
    let ch = received_char;

    let NodeContext {
        old_caret_pos,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt: _,
        ast_node_id: _,
    } = get_node_context(ed_model)?;

    let outcome = match def_ref {
        Def2::Blank { .. } => match ch {
            'a'..='z' => start_new_tld_value(ed_model, ch)?,
            _ => InputOutcome::Ignored,
        },
        Def2::ValueDef { .. } => {
            if curr_mark_node.get_content() == EQUALS {
                let node_caret_offset = ed_model
                    .grid_node_map
                    .get_offset_to_node_id(old_caret_pos, curr_mark_node_id)?;

                if node_caret_offset == 0 || node_caret_offset == EQUALS.len() {
                    let prev_mark_node_id_opt = ed_model.get_prev_mark_node_id()?;

                    if let Some(prev_mark_node_id) = prev_mark_node_id_opt {
                        update_tld_val_name(
                            prev_mark_node_id,
                            ed_model.get_caret(), // TODO update for multiple carets
                            ed_model,
                            ch,
                        )?
                    } else {
                        unreachable!()
                    }
                } else {
                    InputOutcome::Ignored
                }
            } else {
                update_tld_val_name(
                    curr_mark_node_id,
                    ed_model.get_caret(), // TODO update for multiple carets
                    ed_model,
                    ch,
                )?
            }
        }
        Def2::CommentsBefore { .. } => {
            todo!()
        }
        Def2::CommentsAfter { .. } => {
            todo!()
        }
    };

    Ok(outcome)
}

// handle new char when the current(caret is here) MarkupNode corresponds to an Expr2 in the AST
pub fn handle_new_char_expr(
    received_char: &char,
    expr_id: ExprId,
    ed_model: &mut EdModel,
) -> EdResult<InputOutcome> {
    let expr_ref = ed_model.module.env.pool.get(expr_id);
    let ch = received_char;

    let NodeContext {
        old_caret_pos: _,
        curr_mark_node_id,
        curr_mark_node,
        parent_id_opt: _,
        ast_node_id: _,
    } = get_node_context(ed_model)?;

    let prev_mark_node_id_opt = ed_model.get_prev_mark_node_id()?;

    let outcome = if let Expr2::Blank { .. } = expr_ref {
        match ch {
            'a'..='z' => start_new_let_value(ed_model, ch)?,
            '"' => start_new_string(ed_model)?,
            '{' => start_new_record(ed_model)?,
            '0'..='9' => start_new_int(ed_model, ch)?,
            '[' => {
                // this can also be a tag union or become a set, assuming list for now
                start_new_list(ed_model)?
            }
            '\r' => {
                println!("For convenience and consistency there is only one way to format Roc, you can't add extra blank lines.");
                InputOutcome::Ignored
            }
            _ => InputOutcome::Ignored,
        }
    } else if let Some(prev_mark_node_id) = prev_mark_node_id_opt {
        if prev_mark_node_id == curr_mark_node_id {
            match expr_ref {
                Expr2::SmallInt { .. } => update_int(ed_model, curr_mark_node_id, ch)?,
                Expr2::SmallStr(old_arr_str) => update_small_string(ch, old_arr_str, ed_model)?,
                Expr2::Str(..) => update_string(*ch, ed_model)?,
                Expr2::InvalidLookup(old_pool_str) => update_invalid_lookup(
                    &ch.to_string(),
                    old_pool_str,
                    curr_mark_node_id,
                    expr_id,
                    ed_model,
                )?,
                Expr2::EmptyRecord => {
                    // prev_mark_node_id and curr_mark_node_id should be different to allow creating field at current caret position
                    InputOutcome::Ignored
                }
                Expr2::Record {
                    record_var: _,
                    fields,
                } => {
                    if curr_mark_node
                        .get_content()
                        .chars()
                        .all(|chr| chr.is_ascii_alphanumeric())
                    {
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
                _ => InputOutcome::Ignored,
            }
        } else if ch.is_ascii_alphanumeric() {
            // prev_mark_node_id != curr_mark_node_id

            match expr_ref {
                Expr2::SmallInt { .. } => update_int(ed_model, curr_mark_node_id, ch)?,
                _ => {
                    let prev_ast_node_id = ed_model.mark_id_ast_id_map.get(prev_mark_node_id)?;

                    match prev_ast_node_id {
                        ASTNodeId::ADefId(_) => InputOutcome::Ignored,
                        ASTNodeId::AExprId(prev_expr_id) => {
                            handle_new_char_diff_mark_nodes_prev_is_expr(
                                ch,
                                prev_expr_id,
                                expr_id,
                                prev_mark_node_id,
                                curr_mark_node_id,
                                ed_model,
                            )?
                        }
                    }
                }
            }
        } else if *ch == ':' {
            let mark_parent_id_opt = curr_mark_node.get_parent_id_opt();

            if let Some(mark_parent_id) = mark_parent_id_opt {
                let parent_ast_id = ed_model.mark_id_ast_id_map.get(mark_parent_id)?;

                match parent_ast_id {
                    ASTNodeId::ADefId(_) => InputOutcome::Ignored,
                    ASTNodeId::AExprId(parent_expr_id) => {
                        update_record_colon(ed_model, parent_expr_id)?
                    }
                }
            } else {
                InputOutcome::Ignored
            }
        } else if *ch == ',' {
            if curr_mark_node.get_content() == nodes::LEFT_SQUARE_BR {
                InputOutcome::Ignored
            } else {
                let mark_parent_id_opt = curr_mark_node.get_parent_id_opt();

                if let Some(mark_parent_id) = mark_parent_id_opt {
                    let parent_ast_id = ed_model.mark_id_ast_id_map.get(mark_parent_id)?;

                    match parent_ast_id {
                        ASTNodeId::ADefId(_) => InputOutcome::Ignored,
                        ASTNodeId::AExprId(parent_expr_id) => {
                            let parent_expr2 = ed_model.module.env.pool.get(parent_expr_id);

                            match parent_expr2 {
                                Expr2::List {
                                    elem_var: _,
                                    elems: _,
                                } => {
                                    let (new_child_index, new_ast_child_index) =
                                        ed_model.get_curr_child_indices()?;
                                    // insert a Blank first, this results in cleaner code
                                    add_blank_child(new_child_index, new_ast_child_index, ed_model)?
                                }
                                Expr2::Record {
                                    record_var: _,
                                    fields: _,
                                } => {
                                    todo!("multiple record fields")
                                }
                                _ => InputOutcome::Ignored,
                            }
                        }
                    }
                } else {
                    InputOutcome::Ignored
                }
            }
        } else if "\"{[".contains(*ch) {
            let prev_mark_node = ed_model.mark_node_pool.get(prev_mark_node_id);

            if prev_mark_node.get_content() == nodes::LEFT_SQUARE_BR
                && curr_mark_node.get_content() == nodes::RIGHT_SQUARE_BR
            {
                let (new_child_index, new_ast_child_index) = ed_model.get_curr_child_indices()?;
                // insert a Blank first, this results in cleaner code
                add_blank_child(new_child_index, new_ast_child_index, ed_model)?;
                ed_model.post_process_ast_update()?;
                handle_new_char(received_char, ed_model)?
            } else {
                InputOutcome::Ignored
            }
        } else {
            InputOutcome::Ignored
        }
    } else {
        InputOutcome::Ignored
    };

    Ok(outcome)
}

// handle new char when prev_mark_node != curr_mark_node and prev_mark_node's AST node is an Expr2
pub fn handle_new_char_diff_mark_nodes_prev_is_expr(
    received_char: &char,
    prev_expr_id: ExprId,
    curr_expr_id: ExprId,
    prev_mark_node_id: MarkNodeId,
    curr_mark_node_id: MarkNodeId,
    ed_model: &mut EdModel,
) -> EdResult<InputOutcome> {
    let prev_expr_ref = ed_model.module.env.pool.get(prev_expr_id);
    let curr_expr_ref = ed_model.module.env.pool.get(curr_expr_id);
    let ch = received_char;
    let curr_mark_node = ed_model.mark_node_pool.get(curr_mark_node_id);

    let outcome = match prev_expr_ref {
        Expr2::SmallInt { .. } => update_int(ed_model, prev_mark_node_id, ch)?,
        Expr2::InvalidLookup(old_pool_str) => update_invalid_lookup(
            &ch.to_string(),
            old_pool_str,
            prev_mark_node_id,
            prev_expr_id,
            ed_model,
        )?,
        Expr2::Record {
            record_var: _,
            fields,
        } => {
            let prev_mark_node = ed_model.mark_node_pool.get(prev_mark_node_id);

            if (curr_mark_node.get_content() == nodes::RIGHT_ACCOLADE
                || curr_mark_node.get_content() == nodes::COLON)
                && prev_mark_node.is_all_alphanumeric()
            {
                update_record_field(
                    &ch.to_string(),
                    ed_model.get_caret(),
                    prev_mark_node_id,
                    fields,
                    ed_model,
                )?
            } else if prev_mark_node.get_content() == nodes::LEFT_ACCOLADE
                && curr_mark_node.is_all_alphanumeric()
            {
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
        Expr2::List {
            elem_var: _,
            elems: _,
        } => {
            let prev_mark_node = ed_model.mark_node_pool.get(prev_mark_node_id);

            if prev_mark_node.get_content() == nodes::LEFT_SQUARE_BR
                && curr_mark_node.get_content() == nodes::RIGHT_SQUARE_BR
            {
                // based on if, we are at the start of the list
                let new_child_index = 1;
                let new_ast_child_index = 0;
                // insert a Blank first, this results in cleaner code
                add_blank_child(new_child_index, new_ast_child_index, ed_model)?;
                ed_model.post_process_ast_update()?;
                handle_new_char(received_char, ed_model)?
            } else {
                InputOutcome::Ignored
            }
        }
        _ => match curr_expr_ref {
            Expr2::EmptyRecord => {
                let sibling_ids = curr_mark_node.get_sibling_ids(&ed_model.mark_node_pool);

                update_empty_record(&ch.to_string(), prev_mark_node_id, sibling_ids, ed_model)?
            }
            _ => InputOutcome::Ignored,
        },
    };

    Ok(outcome)
}

// updates the ed_model based on the char the user just typed if the result would be syntactically correct.
pub fn handle_new_char(received_char: &char, ed_model: &mut EdModel) -> EdResult<InputOutcome> {
    //dbg!("{}", ed_model.module.ast.ast_to_string(ed_model.module.env.pool));

    let input_outcome = match received_char {
            '\u{e000}'..='\u{f8ff}' // http://www.unicode.org/faq/private_use.html
            | '\u{f0000}'..='\u{ffffd}' // ^
            | '\u{100000}'..='\u{10fffd}' // ^
            => {
                InputOutcome::Ignored
            }
            '\u{8}' | '\u{7f}' => {
                // On Linux, '\u{8}' is backspace,
                // on macOS '\u{7f}'.

                ed_model.backspace()?;

                InputOutcome::Accepted
            }
            ch => {
                let outcome =
                    if ed_model.node_exists_at_caret() {
                        let curr_mark_node_id = ed_model.get_curr_mark_node_id()?;
                        let ast_node_id = ed_model.mark_id_ast_id_map.get(curr_mark_node_id)?;

                        match ast_node_id {
                            ASTNodeId::ADefId(def_id) => {
                                handle_new_char_def(received_char, def_id, ed_model)?
                            },
                            ASTNodeId::AExprId(expr_id) => {
                                handle_new_char_expr(received_char, expr_id, ed_model)?
                            }
                        }

                    } else { //no MarkupNode at the current position
                            if *received_char == '\r' {
                                break_line(ed_model)?
                            } else {
                                let prev_mark_node_id_opt = ed_model.get_prev_mark_node_id()?;
                                if let Some(prev_mark_node_id) = prev_mark_node_id_opt {

                                    let prev_ast_node_id = ed_model.mark_id_ast_id_map.get(prev_mark_node_id)?.to_expr_id()?;
                                    let prev_ast_node = ed_model.module.env.pool.get(prev_ast_node_id);

                                    match prev_ast_node {
                                        Expr2::SmallInt{ .. } => {
                                            update_int(ed_model, prev_mark_node_id, ch)?
                                        },
                                        _ => {
                                            InputOutcome::Ignored
                                        }
                                    }
                                } else {
                                    match ch {
                                        'a'..='z' => {
                                            for caret_pos in ed_model.get_carets() {

                                                if caret_pos.line > 0 {
                                                    insert_new_blank(ed_model, caret_pos.line)?;
                                                    ed_model.post_process_ast_update()?;
                                                }
                                            }
                                            handle_new_char(received_char, ed_model)?
                                        }
                                        _ => {
                                            InputOutcome::Ignored
                                        }
                                    }
                                }
                            }
                        };


                    if let InputOutcome::Accepted = outcome {
                        ed_model.set_sel_none();
                    }

                    outcome
            }
        };

    if let InputOutcome::Accepted = input_outcome {
        ed_model.post_process_ast_update()?;
        ed_model.dirty = true;
    }

    Ok(input_outcome)
}

#[cfg(test)]
pub mod test_ed_update {
    use std::iter;

    use crate::editor::ed_error::print_err;
    use crate::editor::mvc::ed_model::test_ed_model::ed_model_from_dsl;
    use crate::editor::mvc::ed_model::test_ed_model::ed_model_to_dsl;
    use crate::editor::mvc::ed_model::test_ed_model::init_model_refs;
    use crate::editor::mvc::ed_update::handle_new_char;
    use crate::editor::mvc::ed_update::EdModel;
    use crate::editor::mvc::ed_update::EdResult;
    use crate::editor::resources::strings::nr_hello_world_lines;
    use crate::ui::text::lines::SelectableLines;
    use crate::ui::ui_error::UIResult;
    use crate::window::keyboard_input::no_mods;
    use crate::window::keyboard_input::test_modifiers::ctrl_cmd_shift;
    use crate::window::keyboard_input::Modifiers;
    use bumpalo::Bump;
    use roc_code_markup::markup::common_nodes::NEW_LINES_AFTER_DEF;
    use roc_module::symbol::ModuleIds;
    use threadpool::ThreadPool;
    use winit::event::VirtualKeyCode::*;

    fn ed_res_to_res<T: std::fmt::Debug>(ed_res: EdResult<T>) -> Result<T, String> {
        match ed_res {
            Ok(t) => Ok(t),
            Err(e) => {
                print_err(&e);
                Err(e.to_string())
            }
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
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        new_char: char,
    ) -> Result<(), String> {
        assert_insert_seq(pre_lines, expected_post_lines, &new_char.to_string())
    }

    pub fn assert_insert_nls(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        new_char: char,
    ) -> Result<(), String> {
        assert_insert(pre_lines, add_nls(expected_post_lines), new_char)
    }

    pub fn assert_insert_no_pre(
        expected_post_lines: Vec<String>,
        new_char: char,
    ) -> Result<(), String> {
        assert_insert_seq_no_pre(expected_post_lines, &new_char.to_string())
    }

    pub fn assert_insert_seq_no_pre(
        expected_post_lines: Vec<String>,
        new_char_seq: &str,
    ) -> Result<(), String> {
        assert_insert_seq(vec!["┃".to_owned()], expected_post_lines, new_char_seq)
    }

    // pre-insert `val = `
    pub fn assert_insert_in_def(
        expected_post_lines: Vec<String>,
        new_char: char,
    ) -> Result<(), String> {
        assert_insert_seq_in_def(expected_post_lines, &new_char.to_string())
    }

    // pre-insert `val = `
    pub fn assert_insert_seq_in_def(
        expected_post_lines: Vec<String>,
        new_char_seq: &str,
    ) -> Result<(), String> {
        let prefix = "val🡲🡲🡲";

        let full_input = merge_strings(vec![prefix, new_char_seq]);

        let mut expected_post_lines_vec = expected_post_lines.to_vec();

        let first_line_opt = expected_post_lines_vec.first();
        let val_str = "val = ";

        if let Some(first_line) = first_line_opt {
            expected_post_lines_vec[0] = merge_strings(vec![val_str, first_line]);
        } else {
            expected_post_lines_vec = vec![val_str.to_owned()];
        }

        assert_insert_seq_no_pre(expected_post_lines_vec, &full_input)
    }

    pub fn assert_insert_in_def_nls(
        expected_post_lines: Vec<String>,
        new_char: char,
    ) -> Result<(), String> {
        assert_insert_seq_in_def(add_nls(expected_post_lines), &new_char.to_string())
    }

    // Create ed_model from pre_lines DSL, do handle_new_char() for every char in new_char_seq, check if modified ed_model has expected
    // string representation of code, caret position and active selection.
    pub fn assert_insert_seq(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        new_char_seq: &str,
    ) -> Result<(), String> {
        let mut code_str = pre_lines.join("\n").replace('┃', "");

        let mut model_refs = init_model_refs();
        let code_arena = Bump::new();
        let module_ids = ModuleIds::default();

        let mut ed_model = ed_model_from_dsl(
            &mut code_str,
            pre_lines,
            &mut model_refs,
            &module_ids,
            &code_arena,
        )?;

        for input_char in new_char_seq.chars() {
            if input_char == '🡲' {
                ed_model.simple_move_carets_right(1);
            } else if input_char == '🡰' {
                ed_model.simple_move_carets_left(1);
            } else if input_char == '🡱' {
                ed_model.simple_move_carets_up(1);
            } else {
                //dbg!(input_char);
                ed_res_to_res(handle_new_char(&input_char, &mut ed_model))?;
            }
        }

        let mut post_lines = ui_res_to_res(ed_model_to_dsl(&ed_model))?;
        strip_header(&mut post_lines); // remove header for clean tests

        assert_eq!(post_lines, expected_post_lines);

        Ok(())
    }

    fn strip_header(lines: &mut Vec<String>) {
        lines.drain(0..nr_hello_world_lines());
    }

    pub fn assert_insert_seq_nls(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        new_char_seq: &str,
    ) -> Result<(), String> {
        assert_insert_seq(pre_lines, add_nls(expected_post_lines), new_char_seq)
    }

    pub fn assert_insert_seq_ignore(lines: Vec<String>, new_char_seq: &str) -> Result<(), String> {
        assert_insert_seq(lines.clone(), lines, new_char_seq)
    }

    pub fn assert_insert_seq_ignore_nls(
        lines: Vec<String>,
        new_char_seq: &str,
    ) -> Result<(), String> {
        assert_insert_seq_ignore(add_nls(lines), new_char_seq)
    }

    pub fn assert_insert_ignore(lines: Vec<String>, new_char: char) -> Result<(), String> {
        assert_insert_seq_ignore(lines, &new_char.to_string())
    }

    pub fn assert_insert_ignore_nls(lines: Vec<String>, new_char: char) -> Result<(), String> {
        assert_insert_seq_ignore(add_nls(lines), &new_char.to_string())
    }

    // to create Vec<String> from list of &str
    macro_rules! ovec {
        ( $( $x:expr ),* ) => {
            {
                vec![
                    $(
                        $x.to_owned(),
                    )*
                ]
            }
        };
    }

    #[test]
    fn test_ignore_basic() -> Result<(), String> {
        assert_insert_no_pre(ovec!["┃"], ';')?;
        assert_insert_no_pre(ovec!["┃"], '-')?;
        assert_insert_no_pre(ovec!["┃"], '_')?;
        // extra space because of Expr2::Blank placeholder
        assert_insert_in_def_nls(ovec!["┃ "], ';')?;
        assert_insert_in_def_nls(ovec!["┃ "], '-')?;
        assert_insert_in_def_nls(ovec!["┃ "], '_')?;

        Ok(())
    }

    // add newlines like the editor's formatting would add them
    fn add_nls(lines: Vec<String>) -> Vec<String> {
        let mut new_lines = lines;
        //line(s) between TLD's, extra newline so the user can go to last line add new def there
        let mut extra_empty_lines = iter::repeat("".to_owned())
            .take(NEW_LINES_AFTER_DEF)
            .collect();
        new_lines.append(&mut extra_empty_lines);

        new_lines
    }

    //TODO test_int arch bit limit
    #[test]
    fn test_int() -> Result<(), String> {
        assert_insert_in_def_nls(ovec!["0┃"], '0')?;
        assert_insert_in_def_nls(ovec!["1┃"], '1')?;
        assert_insert_in_def_nls(ovec!["2┃"], '2')?;
        assert_insert_in_def_nls(ovec!["3┃"], '3')?;
        assert_insert_in_def_nls(ovec!["4┃"], '4')?;
        assert_insert_in_def_nls(ovec!["5┃"], '5')?;
        assert_insert_in_def_nls(ovec!["6┃"], '6')?;
        assert_insert_in_def_nls(ovec!["7┃"], '7')?;
        assert_insert_in_def_nls(ovec!["8┃"], '8')?;
        assert_insert_in_def_nls(ovec!["9┃"], '9')?;

        assert_insert(ovec!["val = 1┃"], add_nls(ovec!["val = 19┃"]), '9')?;
        assert_insert(ovec!["val = 9876┃"], add_nls(ovec!["val = 98769┃"]), '9')?;
        assert_insert(ovec!["val = 10┃"], add_nls(ovec!["val = 103┃"]), '3')?;
        assert_insert(ovec!["val = ┃0"], add_nls(ovec!["val = 1┃0"]), '1')?;
        assert_insert(ovec!["val = 10000┃"], add_nls(ovec!["val = 100000┃"]), '0')?;

        assert_insert(ovec!["val = ┃1234"], add_nls(ovec!["val = 5┃1234"]), '5')?;
        assert_insert(ovec!["val = 1┃234"], add_nls(ovec!["val = 10┃234"]), '0')?;
        assert_insert(ovec!["val = 12┃34"], add_nls(ovec!["val = 121┃34"]), '1')?;
        assert_insert(ovec!["val = 123┃4"], add_nls(ovec!["val = 1232┃4"]), '2')?;

        Ok(())
    }

    fn merge_strings(strings: Vec<&str>) -> String {
        strings
            .iter()
            .map(|&some_str| some_str.to_owned())
            .collect::<Vec<String>>()
            .join("")
    }

    const IGNORE_CHARS: &str = "{}()[]-><-_\"azAZ:@09";
    const IGNORE_CHARS_NO_NUM: &str = ",{}()[]-><-_\"azAZ:@";
    const IGNORE_NO_LTR: &str = "{\"5";
    const IGNORE_NO_NUM: &str = "a{\"";

    #[test]
    fn test_ignore_int() -> Result<(), String> {
        assert_insert_seq_ignore_nls(ovec!["vec = ┃0"], IGNORE_CHARS_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["vec = ┃7"], IGNORE_CHARS_NO_NUM)?;

        assert_insert_seq_ignore_nls(ovec!["vec = 0┃"], IGNORE_CHARS_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["vec = 8┃"], IGNORE_CHARS_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["vec = 20┃"], IGNORE_CHARS_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["vec = 83┃"], IGNORE_CHARS_NO_NUM)?;

        assert_insert_seq_ignore_nls(ovec!["vec = 1┃0"], IGNORE_CHARS_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["vec = 8┃4"], IGNORE_CHARS_NO_NUM)?;

        assert_insert_seq_ignore_nls(ovec!["vec = ┃10"], IGNORE_CHARS_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["vec = ┃84"], IGNORE_CHARS_NO_NUM)?;

        assert_insert_seq_ignore_nls(ovec!["vec = 129┃96"], IGNORE_CHARS_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["vec = 97┃684"], IGNORE_CHARS_NO_NUM)?;

        assert_insert_ignore_nls(ovec!["vec = 0┃"], '0')?;
        assert_insert_ignore_nls(ovec!["vec = 0┃"], '9')?;
        assert_insert_ignore_nls(ovec!["vec = ┃0"], '0')?;
        assert_insert_ignore_nls(ovec!["vec = ┃1234"], '0')?;
        assert_insert_ignore_nls(ovec!["vec = ┃100"], '0')?;

        Ok(())
    }

    #[test]
    fn test_string() -> Result<(), String> {
        assert_insert_in_def_nls(ovec!["\"┃\""], '"')?;
        assert_insert(ovec!["val = \"┃\""], add_nls(ovec!["val = \"a┃\""]), 'a')?;
        assert_insert(ovec!["val = \"┃\""], add_nls(ovec!["val = \"{┃\""]), '{')?;
        assert_insert(ovec!["val = \"┃\""], add_nls(ovec!["val = \"}┃\""]), '}')?;
        assert_insert(ovec!["val = \"┃\""], add_nls(ovec!["val = \"[┃\""]), '[')?;
        assert_insert(ovec!["val = \"┃\""], add_nls(ovec!["val = \"]┃\""]), ']')?;
        assert_insert(ovec!["val = \"┃\""], add_nls(ovec!["val = \"-┃\""]), '-')?;
        assert_insert(ovec!["val = \"┃-\""], add_nls(ovec!["val = \"<┃-\""]), '<')?;
        assert_insert(ovec!["val = \"-┃\""], add_nls(ovec!["val = \"->┃\""]), '>')?;

        assert_insert(ovec!["val = \"a┃\""], add_nls(ovec!["val = \"ab┃\""]), 'b')?;
        assert_insert(
            ovec!["val = \"ab┃\""],
            add_nls(ovec!["val = \"abc┃\""]),
            'c',
        )?;
        assert_insert(ovec!["val = \"┃a\""], add_nls(ovec!["val = \"z┃a\""]), 'z')?;
        assert_insert(ovec!["val = \"┃a\""], add_nls(ovec!["val = \" ┃a\""]), ' ')?;
        assert_insert(
            ovec!["val = \"a┃b\""],
            add_nls(ovec!["val = \"az┃b\""]),
            'z',
        )?;
        assert_insert(
            ovec!["val = \"a┃b\""],
            add_nls(ovec!["val = \"a ┃b\""]),
            ' ',
        )?;

        assert_insert(
            ovec!["val = \"ab ┃\""],
            add_nls(ovec!["val = \"ab {┃\""]),
            '{',
        )?;
        assert_insert(
            ovec!["val = \"ab ┃\""],
            add_nls(ovec!["val = \"ab }┃\""]),
            '}',
        )?;
        assert_insert(
            ovec!["val = \"{ str: 4┃}\""],
            add_nls(ovec!["val = \"{ str: 44┃}\""]),
            '4',
        )?;
        assert_insert(
            ovec!["val = \"┃ello, hello, hello\""],
            add_nls(ovec!["val = \"h┃ello, hello, hello\""]),
            'h',
        )?;
        assert_insert(
            ovec!["val = \"hello┃ hello, hello\""],
            add_nls(ovec!["val = \"hello,┃ hello, hello\""]),
            ',',
        )?;
        assert_insert(
            ovec!["val = \"hello, hello, hello┃\""],
            add_nls(ovec!["val = \"hello, hello, hello.┃\""]),
            '.',
        )?;

        Ok(())
    }

    #[test]
    fn test_ignore_string() -> Result<(), String> {
        assert_insert_ignore(add_nls(ovec!["val = ┃\"\""]), 'a')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"\""]), 'A')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"\""]), '"')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"\""]), '{')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"\""]), '[')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"\""]), '}')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"\""]), ']')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"\""]), '-')?;

        assert_insert_ignore(add_nls(ovec!["val = \"\"┃"]), 'a')?;
        assert_insert_ignore(add_nls(ovec!["val = \"\"┃"]), 'A')?;
        assert_insert_ignore(add_nls(ovec!["val = \"\"┃"]), '"')?;
        assert_insert_ignore(add_nls(ovec!["val = \"\"┃"]), '{')?;
        assert_insert_ignore(add_nls(ovec!["val = \"\"┃"]), '[')?;
        assert_insert_ignore(add_nls(ovec!["val = \"\"┃"]), '}')?;
        assert_insert_ignore(add_nls(ovec!["val = \"\"┃"]), ']')?;
        assert_insert_ignore(add_nls(ovec!["val = \"\"┃"]), '-')?;

        assert_insert_ignore(add_nls(ovec!["val = ┃\"a\""]), 'a')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"a\""]), 'A')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"a\""]), '"')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"a\""]), '{')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"a\""]), '[')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"a\""]), '}')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"a\""]), ']')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"a\""]), '-')?;

        assert_insert_ignore(add_nls(ovec!["val = \"a\"┃"]), 'a')?;
        assert_insert_ignore(add_nls(ovec!["val = \"a\"┃"]), 'A')?;
        assert_insert_ignore(add_nls(ovec!["val = \"a\"┃"]), '"')?;
        assert_insert_ignore(add_nls(ovec!["val = \"a\"┃"]), '{')?;
        assert_insert_ignore(add_nls(ovec!["val = \"a\"┃"]), '[')?;
        assert_insert_ignore(add_nls(ovec!["val = \"a\"┃"]), '}')?;
        assert_insert_ignore(add_nls(ovec!["val = \"a\"┃"]), ']')?;
        assert_insert_ignore(add_nls(ovec!["val = \"a\"┃"]), '-')?;

        assert_insert_ignore(add_nls(ovec!["val = ┃\"{  }\""]), 'a')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"{  }\""]), 'A')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"{  }\""]), '"')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"{  }\""]), '{')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"{  }\""]), '[')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"{  }\""]), '}')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"{  }\""]), ']')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"{  }\""]), '-')?;

        assert_insert_ignore(add_nls(ovec!["val = \"{  }\"┃"]), 'A')?;
        assert_insert_ignore(add_nls(ovec!["val = \"{  }\"┃"]), 'a')?;
        assert_insert_ignore(add_nls(ovec!["val = \"{  }\"┃"]), '"')?;
        assert_insert_ignore(add_nls(ovec!["val = \"{  }\"┃"]), '{')?;
        assert_insert_ignore(add_nls(ovec!["val = \"{  }\"┃"]), '[')?;
        assert_insert_ignore(add_nls(ovec!["val = \"{  }\"┃"]), '}')?;
        assert_insert_ignore(add_nls(ovec!["val = \"{  }\"┃"]), ']')?;
        assert_insert_ignore(add_nls(ovec!["val = \"{  }\"┃"]), '-')?;

        assert_insert_ignore(add_nls(ovec!["val = \"[ 1, 2, 3 ]\"┃"]), '{')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"[ 1, 2, 3 ]\""]), '{')?;
        assert_insert_ignore(add_nls(ovec!["val = \"hello, hello, hello\"┃"]), '.')?;
        assert_insert_ignore(add_nls(ovec!["val = ┃\"hello, hello, hello\""]), '.')?;

        Ok(())
    }

    #[test]
    fn test_record() -> Result<(), String> {
        assert_insert_in_def_nls(ovec!["{ ┃ }"], '{')?;
        assert_insert_nls(ovec!["val = { ┃ }"], ovec!["val = { a┃ }"], 'a')?;
        assert_insert_nls(ovec!["val = { a┃ }"], ovec!["val = { ab┃ }"], 'b')?;
        assert_insert_nls(ovec!["val = { a┃ }"], ovec!["val = { a1┃ }"], '1')?;
        assert_insert_nls(ovec!["val = { a1┃ }"], ovec!["val = { a1z┃ }"], 'z')?;
        assert_insert_nls(ovec!["val = { a1┃ }"], ovec!["val = { a15┃ }"], '5')?;
        assert_insert_nls(ovec!["val = { ab┃ }"], ovec!["val = { abc┃ }"], 'c')?;
        assert_insert_nls(ovec!["val = { ┃abc }"], ovec!["val = { z┃abc }"], 'z')?;
        assert_insert_nls(ovec!["val = { a┃b }"], ovec!["val = { az┃b }"], 'z')?;
        assert_insert_nls(ovec!["val = { a┃b }"], ovec!["val = { a9┃b }"], '9')?;

        assert_insert_nls(ovec!["val = { a┃ }"], ovec!["val = { a: ┃  }"], ':')?;
        assert_insert_nls(ovec!["val = { abc┃ }"], ovec!["val = { abc: ┃  }"], ':')?;
        assert_insert_nls(ovec!["val = { aBc┃ }"], ovec!["val = { aBc: ┃  }"], ':')?;

        assert_insert_seq_nls(ovec!["val = { a┃ }"], ovec!["val = { a: \"┃\" }"], ":\"")?;
        assert_insert_seq_nls(
            ovec!["val = { abc┃ }"],
            ovec!["val = { abc: \"┃\" }"],
            ":\"",
        )?;

        assert_insert_seq_nls(ovec!["val = { a┃ }"], ovec!["val = { a: 0┃ }"], ":0")?;
        assert_insert_seq_nls(ovec!["val = { abc┃ }"], ovec!["val = { abc: 9┃ }"], ":9")?;
        assert_insert_seq_nls(ovec!["val = { a┃ }"], ovec!["val = { a: 1000┃ }"], ":1000")?;
        assert_insert_seq_nls(
            ovec!["val = { abc┃ }"],
            ovec!["val = { abc: 98761┃ }"],
            ":98761",
        )?;

        assert_insert_nls(
            ovec!["val = { a: \"┃\" }"],
            ovec!["val = { a: \"a┃\" }"],
            'a',
        )?;
        assert_insert_nls(
            ovec!["val = { a: \"a┃\" }"],
            ovec!["val = { a: \"ab┃\" }"],
            'b',
        )?;
        assert_insert_nls(
            ovec!["val = { a: \"a┃b\" }"],
            ovec!["val = { a: \"az┃b\" }"],
            'z',
        )?;
        assert_insert_nls(
            ovec!["val = { a: \"┃ab\" }"],
            ovec!["val = { a: \"z┃ab\" }"],
            'z',
        )?;

        assert_insert_nls(ovec!["val = { a: 1┃ }"], ovec!["val = { a: 10┃ }"], '0')?;
        assert_insert_nls(ovec!["val = { a: 100┃ }"], ovec!["val = { a: 1004┃ }"], '4')?;
        assert_insert_nls(ovec!["val = { a: 9┃76 }"], ovec!["val = { a: 98┃76 }"], '8')?;
        assert_insert_nls(
            ovec!["val = { a: 4┃691 }"],
            ovec!["val = { a: 40┃691 }"],
            '0',
        )?;
        assert_insert_nls(
            ovec!["val = { a: 469┃1 }"],
            ovec!["val = { a: 4699┃1 }"],
            '9',
        )?;

        assert_insert_nls(
            ovec!["val = { camelCase: \"┃\" }"],
            ovec!["val = { camelCase: \"a┃\" }"],
            'a',
        )?;
        assert_insert_nls(
            ovec!["val = { camelCase: \"a┃\" }"],
            ovec!["val = { camelCase: \"ab┃\" }"],
            'b',
        )?;

        assert_insert_nls(
            ovec!["val = { camelCase: 3┃ }"],
            ovec!["val = { camelCase: 35┃ }"],
            '5',
        )?;
        assert_insert_nls(
            ovec!["val = { camelCase: ┃2 }"],
            ovec!["val = { camelCase: 5┃2 }"],
            '5',
        )?;
        assert_insert_nls(
            ovec!["val = { camelCase: 10┃2 }"],
            ovec!["val = { camelCase: 106┃2 }"],
            '6',
        )?;

        assert_insert_nls(
            ovec!["val = { a┃: \"\" }"],
            ovec!["val = { ab┃: \"\" }"],
            'b',
        )?;
        assert_insert_nls(
            ovec!["val = { ┃a: \"\" }"],
            ovec!["val = { z┃a: \"\" }"],
            'z',
        )?;
        assert_insert_nls(
            ovec!["val = { ab┃: \"\" }"],
            ovec!["val = { abc┃: \"\" }"],
            'c',
        )?;
        assert_insert_nls(
            ovec!["val = { ┃ab: \"\" }"],
            ovec!["val = { z┃ab: \"\" }"],
            'z',
        )?;
        assert_insert_nls(
            ovec!["val = { camelCase┃: \"hello\" }"],
            ovec!["val = { camelCaseB┃: \"hello\" }"],
            'B',
        )?;
        assert_insert_nls(
            ovec!["val = { camel┃Case: \"hello\" }"],
            ovec!["val = { camelZ┃Case: \"hello\" }"],
            'Z',
        )?;
        assert_insert_nls(
            ovec!["val = { ┃camelCase: \"hello\" }"],
            ovec!["val = { z┃camelCase: \"hello\" }"],
            'z',
        )?;

        assert_insert_nls(ovec!["val = { a┃: 0 }"], ovec!["val = { ab┃: 0 }"], 'b')?;
        assert_insert_nls(
            ovec!["val = { ┃a: 2100 }"],
            ovec!["val = { z┃a: 2100 }"],
            'z',
        )?;
        assert_insert_nls(
            ovec!["val = { ab┃: 9876 }"],
            ovec!["val = { abc┃: 9876 }"],
            'c',
        )?;
        assert_insert_nls(
            ovec!["val = { ┃ab: 102 }"],
            ovec!["val = { z┃ab: 102 }"],
            'z',
        )?;
        assert_insert_nls(
            ovec!["val = { camelCase┃: 99999 }"],
            ovec!["val = { camelCaseB┃: 99999 }"],
            'B',
        )?;
        assert_insert_nls(
            ovec!["val = { camel┃Case: 88156 }"],
            ovec!["val = { camelZ┃Case: 88156 }"],
            'Z',
        )?;
        assert_insert_nls(
            ovec!["val = { ┃camelCase: 1 }"],
            ovec!["val = { z┃camelCase: 1 }"],
            'z',
        )?;

        assert_insert_seq_nls(
            ovec!["val = { ┃ }"],
            ovec!["val = { camelCase: \"hello┃\" }"],
            "camelCase:\"hello",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { ┃ }"],
            ovec!["val = { camelCase: 10009┃ }"],
            "camelCase:10009",
        )?;

        Ok(())
    }

    #[test]
    fn test_nested_record() -> Result<(), String> {
        assert_insert_seq_nls(ovec!["val = { a┃ }"], ovec!["val = { a: { ┃ } }"], ":{")?;
        assert_insert_seq_nls(ovec!["val = { abc┃ }"], ovec!["val = { abc: { ┃ } }"], ":{")?;
        assert_insert_seq_nls(
            ovec!["val = { camelCase┃ }"],
            ovec!["val = { camelCase: { ┃ } }"],
            ":{",
        )?;

        assert_insert_seq_nls(
            ovec!["val = { a: { ┃ } }"],
            ovec!["val = { a: { zulu┃ } }"],
            "zulu",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { abc: { ┃ } }"],
            ovec!["val = { abc: { camelCase┃ } }"],
            "camelCase",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { camelCase: { ┃ } }"],
            ovec!["val = { camelCase: { z┃ } }"],
            "z",
        )?;

        assert_insert_seq_nls(
            ovec!["val = { a: { zulu┃ } }"],
            ovec!["val = { a: { zulu: ┃  } }"],
            ":",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { abc: { camelCase┃ } }"],
            ovec!["val = { abc: { camelCase: ┃  } }"],
            ":",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { camelCase: { z┃ } }"],
            ovec!["val = { camelCase: { z: ┃  } }"],
            ":",
        )?;

        assert_insert_seq_nls(
            ovec!["val = { a┃: { zulu } }"],
            ovec!["val = { a0┃: { zulu } }"],
            "0",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { ab┃c: { camelCase } }"],
            ovec!["val = { abz┃c: { camelCase } }"],
            "z",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { ┃camelCase: { z } }"],
            ovec!["val = { x┃camelCase: { z } }"],
            "x",
        )?;

        assert_insert_seq_nls(
            ovec!["val = { a: { zulu┃ } }"],
            ovec!["val = { a: { zulu: \"┃\" } }"],
            ":\"",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { abc: { camelCase┃ } }"],
            ovec!["val = { abc: { camelCase: \"┃\" } }"],
            ":\"",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { camelCase: { z┃ } }"],
            ovec!["val = { camelCase: { z: \"┃\" } }"],
            ":\"",
        )?;

        assert_insert_seq_nls(
            ovec!["val = { a: { zulu: \"┃\" } }"],
            ovec!["val = { a: { zulu: \"azula┃\" } }"],
            "azula",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { a: { zulu: \"az┃a\" } }"],
            ovec!["val = { a: { zulu: \"azul┃a\" } }"],
            "ul",
        )?;

        assert_insert_seq_nls(
            ovec!["val = { a: { zulu┃ } }"],
            ovec!["val = { a: { zulu: 1┃ } }"],
            ":1",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { abc: { camelCase┃ } }"],
            ovec!["val = { abc: { camelCase: 0┃ } }"],
            ":0",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { camelCase: { z┃ } }"],
            ovec!["val = { camelCase: { z: 45┃ } }"],
            ":45",
        )?;

        assert_insert_seq_nls(
            ovec!["val = { a: { zulu: ┃0 } }"],
            ovec!["val = { a: { zulu: 4┃0 } }"],
            "4",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { a: { zulu: 10┃98 } }"],
            ovec!["val = { a: { zulu: 1077┃98 } }"],
            "77",
        )?;

        assert_insert_seq_nls(
            ovec!["val = { a: { zulu┃ } }"],
            ovec!["val = { a: { zulu: { ┃ } } }"],
            ":{",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { abc: { camelCase┃ } }"],
            ovec!["val = { abc: { camelCase: { ┃ } } }"],
            ":{",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { camelCase: { z┃ } }"],
            ovec!["val = { camelCase: { z: { ┃ } } }"],
            ":{",
        )?;

        assert_insert_seq_nls(
            ovec!["val = { a: { zulu: { ┃ } } }"],
            ovec!["val = { a: { zulu: { he┃ } } }"],
            "he",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { a: { ┃zulu: {  } } }"],
            ovec!["val = { a: { x┃zulu: {  } } }"],
            "x",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { a: { z┃ulu: {  } } }"],
            ovec!["val = { a: { z9┃ulu: {  } } }"],
            "9",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { a: { zulu┃: {  } } }"],
            ovec!["val = { a: { zulu7┃: {  } } }"],
            "7",
        )?;

        assert_insert_seq_nls(
            ovec!["val = { a┃: { bcD: { eFgHij: { k15 } } } }"],
            ovec!["val = { a4┃: { bcD: { eFgHij: { k15 } } } }"],
            "4",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { ┃a: { bcD: { eFgHij: { k15 } } } }"],
            ovec!["val = { y┃a: { bcD: { eFgHij: { k15 } } } }"],
            "y",
        )?;
        assert_insert_seq_nls(
            ovec!["val = { a: { bcD: { eF┃gHij: { k15 } } } }"],
            ovec!["val = { a: { bcD: { eFxyz┃gHij: { k15 } } } }"],
            "xyz",
        )?;

        assert_insert_seq_nls(
            ovec!["val = { ┃ }"],
            ovec!["val = { g: { oi: { ng: { d: { e: { e: { p: { camelCase┃ } } } } } } } }"],
            "g:{oi:{ng:{d:{e:{e:{p:{camelCase",
        )?;

        Ok(())
    }

    fn concat_strings(str_a: &str, str_b: &str) -> String {
        let mut string_a = str_a.to_owned();

        string_a.push_str(str_b);

        string_a
    }

    #[test]
    fn test_ignore_record() -> Result<(), String> {
        assert_insert_seq_ignore_nls(ovec!["val = ┃{  }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = {  }┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃  }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = {  ┃}"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = { ┃ }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = { ┃a }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = { ┃abc }"], IGNORE_NO_LTR)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃{ a }"], IGNORE_CHARS)?;
        assert_insert_seq_nls(
            ovec!["val = { a┃ }"],
            ovec!["val = { a:┃   }"],
            &concat_strings(":🡰", IGNORE_CHARS),
        )?;
        assert_insert_seq_nls(
            ovec!["val = { a┃ }"],
            ovec!["val = { a:  ┃ }"],
            &concat_strings(":🡲", IGNORE_CHARS),
        )?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ a }"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃{ a15 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ a15 }"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃{ camelCase }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ camelCase }"], IGNORE_CHARS)?;
        assert_insert_seq_nls(
            ovec!["val = { camelCase┃ }"],
            ovec!["val = { camelCase:┃   }"],
            &concat_strings(":🡰", IGNORE_CHARS),
        )?;
        assert_insert_seq_nls(
            ovec!["val = { camelCase┃ }"],
            ovec!["val = { camelCase:  ┃ }"],
            &concat_strings(":🡲", IGNORE_CHARS),
        )?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃{ a: \"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ a: \"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: ┃\"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: \"\"┃ }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: \"\" }┃"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃{ a: 1 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ a: 2 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: ┃6 }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: 8┃ }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: 0 }┃"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃{ camelCase: 1 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ camelCase: 7 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCase: ┃2 }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCase: 4┃ }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCase: 9 }┃"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃{ camelCase: \"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ camelCase: \"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCase: ┃\"\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCase: \"\"┃ }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCase: \"\" }┃"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃{ a: \"z\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ a: \"z\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: ┃\"z\" }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: \"z\"┃ }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: \"z\" }┃"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(
            ovec!["val = ┃{ a: \"hello, hello.0123456789ZXY{}[]-><-\" }"],
            IGNORE_CHARS,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = {┃ a: \"hello, hello.0123456789ZXY{}[]-><-\" }"],
            IGNORE_CHARS,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { a: ┃\"hello, hello.0123456789ZXY{}[]-><-\" }"],
            IGNORE_CHARS,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { a: \"hello, hello.0123456789ZXY{}[]-><-\"┃ }"],
            IGNORE_CHARS,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { a: \"hello, hello.0123456789ZXY{}[]-><-\" }┃"],
            IGNORE_CHARS,
        )?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃{ a: 915480 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ a: 915480 }"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: ┃915480 }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: 915480┃ }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: 915480 }┃"], IGNORE_CHARS)?;

        Ok(())
    }

    #[test]
    fn test_ignore_nested_record() -> Result<(), String> {
        assert_insert_seq_ignore_nls(ovec!["val = { a: { ┃ } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: ┃{  } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: {┃  } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: {  }┃ }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: {  } ┃}"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a: {  } }┃"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = { a:┃ {  } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ a: {  } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = ┃{ a: {  } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = { ┃a: {  } }"], "1")?;

        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1: {┃ z15a } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1: ┃{ z15a } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_nls(
            ovec!["val = { camelCaseB1: { z15a┃ } }"],
            ovec!["val = { camelCaseB1: { z15a:┃   } }"],
            &concat_strings(":🡰", IGNORE_CHARS),
        )?;
        assert_insert_seq_nls(
            ovec!["val = { camelCaseB1: { z15a┃ } }"],
            ovec!["val = { camelCaseB1: { z15a:  ┃ } }"],
            &concat_strings(":🡲", IGNORE_CHARS),
        )?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1:┃ { z15a } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ camelCaseB1: { z15a } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = ┃{ camelCaseB1: { z15a } }"], IGNORE_NO_LTR)?;
        assert_insert_seq_ignore_nls(ovec!["val = { ┃camelCaseB1: { z15a } }"], "1")?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1: { ┃z15a } }"], "1")?;

        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: \"\"┃ } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: ┃\"\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a:┃ \"\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: \"\" ┃} }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: {┃ z15a: \"\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: ┃{ z15a: \"\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: \"\" }┃ }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: \"\" } ┃}"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: \"\" } }┃"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1:┃ { z15a: \"\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = {┃ camelCaseB1: { z15a: \"\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = ┃{ camelCaseB1: { z15a: \"\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(ovec!["val = { ┃camelCaseB1: { z15a: \"\" } }"], "1")?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1: { ┃z15a: \"\" } }"], "1")?;

        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1: { z15a: 0┃ } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: ┃123 } }"],
            IGNORE_NO_NUM,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a:┃ 999 } }"],
            IGNORE_NO_NUM,
        )?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1: { z15a: 80 ┃} }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: {┃ z15a: 99000 } }"],
            IGNORE_NO_NUM,
        )?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1: ┃{ z15a: 12 } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1: { z15a: 7 }┃ }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1: { z15a: 98 } ┃}"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: 4582 } }┃"],
            IGNORE_NO_NUM,
        )?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1:┃ { z15a: 0 } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["val = {┃ camelCaseB1: { z15a: 44 } }"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(
            ovec!["val = ┃{ camelCaseB1: { z15a: 100123 } }"],
            IGNORE_NO_NUM,
        )?;
        assert_insert_seq_ignore_nls(ovec!["val = { ┃camelCaseB1: { z15a: 5 } }"], "1")?;
        assert_insert_seq_ignore_nls(ovec!["val = { camelCaseB1: { ┃z15a: 6 } }"], "1")?;

        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\"┃ } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: ┃\"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a:┃ \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" ┃} }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: {┃ z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: ┃{ z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" }┃ }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } ┃}"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }┃"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1:┃ { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = {┃ camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = ┃{ camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { ┃camelCaseB1: { z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            "1",
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { camelCaseB1: { ┃z15a: \"hello, hello.0123456789ZXY{}[]-><-\" } }"],
            "1",
        )?;

        assert_insert_seq_ignore_nls(
            ovec!["val = { g: { oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } } }┃"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { g: { oi: { ng: { d: { e: {┃ e: { p: { camelCase } } } } } } } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { g: { oi: { ng: { d: { e: { e:┃ { p: { camelCase } } } } } } } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = {┃ g: { oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = ┃{ g: { oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } } }"],
            IGNORE_NO_LTR,
        )?;
        assert_insert_seq_ignore_nls(
            ovec!["val = { ┃g: { oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } } }"],
            "2",
        )?;
        Ok(())
    }

    #[test]
    fn test_single_elt_list() -> Result<(), String> {
        assert_insert_in_def_nls(ovec!["[ ┃ ]"], '[')?;

        assert_insert_nls(ovec!["val = [ ┃ ]"], ovec!["val = [ 0┃ ]"], '0')?;
        assert_insert_nls(ovec!["val = [ ┃ ]"], ovec!["val = [ 1┃ ]"], '1')?;
        assert_insert_nls(ovec!["val = [ ┃ ]"], ovec!["val = [ 9┃ ]"], '9')?;

        assert_insert_nls(ovec!["val = [ ┃ ]"], ovec!["val = [ \"┃\" ]"], '\"')?;
        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ \"hello, hello.0123456789ZXY{}[]-><-┃\" ]"],
            "\"hello, hello.0123456789ZXY{}[]-><-",
        )?;

        assert_insert_nls(ovec!["val = [ ┃ ]"], ovec!["val = [ { ┃ } ]"], '{')?;
        assert_insert_seq_nls(ovec!["val = [ ┃ ]"], ovec!["val = [ { a┃ } ]"], "{a")?;
        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ { camelCase: { zulu: \"nested┃\" } } ]"],
            "{camelCase:{zulu:\"nested",
        )?;

        assert_insert_nls(ovec!["val = [ ┃ ]"], ovec!["val = [ [ ┃ ] ]"], '[')?;
        assert_insert_seq_nls(ovec!["val = [ ┃ ]"], ovec!["val = [ [ [ ┃ ] ] ]"], "[[")?;
        assert_insert_seq_nls(ovec!["val = [ ┃ ]"], ovec!["val = [ [ 0┃ ] ]"], "[0")?;
        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ [ \"abc┃\" ] ]"],
            "[\"abc",
        )?;
        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ [ { camelCase: { a: 79000┃ } } ] ]"],
            "[{camelCase:{a:79000",
        )?;

        Ok(())
    }

    #[test]
    fn test_ignore_single_elt_list() -> Result<(), String> {
        assert_insert_seq_ignore_nls(ovec!["val = ┃[  ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [  ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [┃  ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [  ┃]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃[ 0 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 0 ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [┃ 0 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 0 ┃]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃[ 137 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 137 ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [┃ 137 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 137 ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ ┃137 ]"], IGNORE_NO_NUM)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 137┃ ]"], IGNORE_NO_NUM)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃[ \"teststring\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ \"teststring\" ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [┃ \"teststring\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ \"teststring\" ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ ┃\"teststring\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ \"teststring\"┃ ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃[ { a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ { a: 1 } ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [┃ { a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ { a: 1 } ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ ┃{ a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ {┃ a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ { a:┃ 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ { a: 1 ┃} ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ { a: 1 }┃ ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃[ [  ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [  ] ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [┃ [  ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [  ] ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ ┃[  ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [  ]┃ ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [┃  ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [  ┃] ]"], IGNORE_CHARS)?;

        Ok(())
    }

    #[test]
    fn test_multi_elt_list() -> Result<(), String> {
        assert_insert_seq_nls(ovec!["val = [ ┃ ]"], ovec!["val = [ 0, 1┃ ]"], "0,1")?;
        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ 987, 6543, 210┃ ]"],
            "987,6543,210",
        )?;

        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ \"a\", \"bcd\", \"EFGH┃\" ]"],
            "\"a🡲,\"bcd🡲,\"EFGH",
        )?;

        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ { a: 1 }, { b: 23 }, { c: 456┃ } ]"],
            "{a:1🡲🡲,{b:23🡲🡲,{c:456",
        )?;

        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ [ 1 ], [ 23 ], [ 456┃ ] ]"],
            "[1🡲🡲,[23🡲🡲,[456",
        )?;

        // insert element in between
        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ 0, 2┃, 1 ]"],
            "0,1🡰🡰🡰,2",
        )?;
        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ 0, 2, 3┃, 1 ]"],
            "0,1🡰🡰🡰,2,3",
        )?;
        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ 0, 3┃, 2, 1 ]"],
            "0,1🡰🡰🡰,2🡰🡰🡰,3",
        )?;

        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ \"abc\", \"f┃\", \"de\" ]"],
            "\"abc🡲,\"de🡰🡰🡰🡰🡰,\"f",
        )?;

        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ [ 0 ], [ 2┃ ], [ 1 ] ]"],
            "[0🡲🡲,[1🡰🡰🡰🡰🡰,[2",
        )?;

        assert_insert_seq_nls(
            ovec!["val = [ ┃ ]"],
            ovec!["val = [ { a: 0 }, { a: 2┃ }, { a: 1 } ]"],
            "{a:0🡲🡲,{a:1🡰🡰🡰🡰🡰🡰🡰🡰,{a:2",
        )?;

        Ok(())
    }

    #[test]
    fn test_ignore_multi_elt_list() -> Result<(), String> {
        assert_insert_seq_ignore_nls(ovec!["val = ┃[ 0, 1 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 0, 1 ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [┃ 0, 1 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 0, 1 ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 0,┃ 1 ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃[ 123, 56, 7 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 123, 56, 7 ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [┃ 123, 56, 7 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 123, 56, 7 ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 123,┃ 56, 7 ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ 123, 56,┃ 7 ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃[ \"123\", \"56\", \"7\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ \"123\", \"56\", \"7\" ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [┃ \"123\", \"56\", \"7\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ \"123\", \"56\", \"7\" ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ \"123\",┃ \"56\", \"7\" ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ \"123\", \"56\",┃ \"7\" ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃[ { a: 0 }, { a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ { a: 0 }, { a: 1 } ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [┃ { a: 0 }, { a: 1 } ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ { a: 0 }, { a: 1 } ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ { a: 0 },┃ { a: 1 } ]"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["val = ┃[ [ 0 ], [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [ 0 ], [ 1 ] ]┃"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [┃ [ 0 ], [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [ 0 ], [ 1 ] ┃]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [ 0 ],┃ [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ ┃[ 0 ], [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [ 0 ]┃, [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [┃ 0 ], [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [ 0 ┃], [ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [ 0 ], ┃[ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [ 0 ], [┃ 1 ] ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [ 0 ], [ 1 ]┃ ]"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["val = [ [ 0 ], [ 1 ┃] ]"], IGNORE_CHARS)?;

        Ok(())
    }

    #[test]
    fn test_tld_value() -> Result<(), String> {
        assert_insert_nls(ovec!["┃"], ovec!["a┃ =  "], 'a')?;
        assert_insert_nls(ovec!["┃"], ovec!["m┃ =  "], 'm')?;
        assert_insert_nls(ovec!["┃"], ovec!["z┃ =  "], 'z')?;

        assert_insert_seq_nls(ovec!["┃"], ovec!["ab┃ =  "], "ab")?;
        // TODO see issue #2548
        //assert_insert_seq_nls(ovec!["┃"], ovec!["mainVal┃ =  "], "mainVal")?;
        assert_insert_seq_nls(ovec!["┃"], ovec!["camelCase123┃ =  "], "camelCase123")?;
        assert_insert_seq_nls(ovec!["┃"], ovec!["c137┃ =  "], "c137")?;
        assert_insert_seq_nls(ovec!["┃"], ovec!["c137Bb┃ =  "], "c137Bb")?;
        assert_insert_seq_nls(ovec!["┃"], ovec!["bBbb┃ =  "], "bBbb")?;
        assert_insert_seq_nls(ovec!["┃"], ovec!["cC0Z┃ =  "], "cC0Z")?;

        Ok(())
    }

    #[test]
    fn test_ignore_tld_value() -> Result<(), String> {
        assert_insert_seq_ignore_nls(ovec!["a ┃= 0"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["a =┃ 0"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["aBC ┃= 0"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["aBC =┃ 0"], IGNORE_CHARS)?;

        assert_insert_seq_ignore_nls(ovec!["camelCase123 ┃= 0"], IGNORE_CHARS)?;
        assert_insert_seq_ignore_nls(ovec!["camelCase123 =┃ 0"], IGNORE_CHARS)?;

        Ok(())
    }

    #[test]
    fn test_enter() -> Result<(), String> {
        assert_insert_seq(
            ovec!["┃"],
            add_nls(ovec!["ab = 5", "", "cd = \"good┃\""]),
            "ab🡲🡲🡲5\rcd🡲🡲🡲\"good",
        )?;

        Ok(())
    }

    // Create ed_model from pre_lines DSL, do handle_new_char for every char in input_seq, do ctrl+shift+up as many times as repeat.
    // check if modified ed_model has expected string representation of code, caret position and active selection.
    pub fn assert_ctrl_shift_up_repeat(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        input_seq: &str,
        repeats: usize,
    ) -> Result<(), String> {
        let mut code_str = pre_lines.join("").replace('┃', "");

        let mut model_refs = init_model_refs();
        let code_arena = Bump::new();
        let module_ids = ModuleIds::default();

        let mut ed_model = ed_model_from_dsl(
            &mut code_str,
            pre_lines,
            &mut model_refs,
            &module_ids,
            &code_arena,
        )?;

        for input_char in input_seq.chars() {
            if input_char == '🡲' {
                ed_model.simple_move_carets_right(1);
            } else if input_char == '🡰' {
                ed_model.simple_move_carets_left(1);
            } else if input_char == '🡱' {
                ed_model.simple_move_carets_up(1);
            } else {
                //dbg!(input_char);
                ed_res_to_res(handle_new_char(&input_char, &mut ed_model))?;
            }
        }

        for _ in 0..repeats {
            ed_model.ed_handle_key_down(&ctrl_cmd_shift(), Up, &mut ThreadPool::new(1))?;
        }

        let mut post_lines = ui_res_to_res(ed_model_to_dsl(&ed_model))?;
        strip_header(&mut post_lines); // remove header for clean tests

        assert_eq!(post_lines, add_nls(expected_post_lines));

        Ok(())
    }

    pub fn assert_ctrl_shift_up_no_inp(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
    ) -> Result<(), String> {
        assert_ctrl_shift_up_repeat(pre_lines, expected_post_lines, "", 1)
    }

    pub fn assert_ctrl_shift_up_repeat_no_inp(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        repeats: usize,
    ) -> Result<(), String> {
        assert_ctrl_shift_up_repeat(pre_lines, expected_post_lines, "", repeats)
    }

    #[test]
    fn test_ctrl_shift_up_blank() -> Result<(), String> {
        // Blank is auto-inserted when creating top level def
        assert_ctrl_shift_up_repeat(ovec!["┃"], ovec!["val = ┃❮ ❯"], "val=🡲🡲🡲", 1)?;
        assert_ctrl_shift_up_repeat(ovec!["┃"], ovec!["┃❮val =  ❯"], "val=🡲🡲🡲", 4)?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_int() -> Result<(), String> {
        assert_ctrl_shift_up_no_inp(ovec!["val = 5┃"], ovec!["val = ┃❮5❯"])?;
        assert_ctrl_shift_up_repeat_no_inp(ovec!["val = 0┃"], ovec!["┃❮val = 0❯"], 4)?;
        assert_ctrl_shift_up_no_inp(ovec!["val = 12345┃"], ovec!["val = ┃❮12345❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = ┃12345"], ovec!["val = ┃❮12345❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = 1┃2345"], ovec!["val = ┃❮12345❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = 12┃345"], ovec!["val = ┃❮12345❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = 123┃45"], ovec!["val = ┃❮12345❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = 1234┃5"], ovec!["val = ┃❮12345❯"])?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_string() -> Result<(), String> {
        assert_ctrl_shift_up_no_inp(ovec!["val = \"┃\""], ovec!["val = ┃❮\"\"❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = ┃\"\""], ovec!["val = ┃❮\"\"❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = \"┃0\""], ovec!["val = ┃❮\"0\"❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = \"0┃\""], ovec!["val = ┃❮\"0\"❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = \"abc┃\""], ovec!["val = ┃❮\"abc\"❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = \"ab┃c\""], ovec!["val = ┃❮\"abc\"❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = \"┃abc\""], ovec!["val = ┃❮\"abc\"❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = ┃\"abc\""], ovec!["val = ┃❮\"abc\"❯"])?;
        assert_ctrl_shift_up_repeat_no_inp(ovec!["val = \"abc┃\""], ovec!["┃❮val = \"abc\"❯"], 4)?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = \"hello, hello.0123456789ZXY{}[]-><-┃\""],
            ovec!["val = ┃❮\"hello, hello.0123456789ZXY{}[]-><-\"❯"],
        )?;

        assert_ctrl_shift_up_no_inp(ovec!["val = \"\"┃"], ovec!["val = ┃❮\"\"❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = \"abc\"┃"], ovec!["val = ┃❮\"abc\"❯"])?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_record() -> Result<(), String> {
        assert_ctrl_shift_up_no_inp(ovec!["val = { ┃ }"], ovec!["val = ┃❮{  }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = {┃  }"], ovec!["val = ┃❮{  }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = ┃{  }"], ovec!["val = ┃❮{  }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = {  ┃}"], ovec!["val = ┃❮{  }❯"])?;
        assert_ctrl_shift_up_repeat_no_inp(ovec!["val = { ┃ }"], ovec!["┃❮val = {  }❯"], 4)?;
        assert_ctrl_shift_up_no_inp(ovec!["val = {  }┃"], ovec!["val = ┃❮{  }❯"])?;
        // TODO uncomment tests once #1649 is fixed
        /*assert_ctrl_shift_up_no_inp(ovec!["val = { pear┃ }"], ovec!["val = ┃❮{ pear }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { pea┃r }"], ovec!["val = ┃❮{ pear }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { p┃ear }"], ovec!["val = ┃❮{ pear }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { ┃pear }"], ovec!["val = ┃❮{ pear }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = {┃ pear }"], ovec!["val = ┃❮{ pear }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = ┃{ pear }"], ovec!["val = ┃❮{ pear }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { pear ┃}"], ovec!["val = ┃❮{ pear }❯"])?;
        assert_ctrl_shift_up_repeat(ovec!["val = { pear┃ }"], ovec!["val = ┃❮{ pear }❯"], 3)?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { pear }┃"], ovec!["val = ┃❮{ pear }❯"])?;

        assert_ctrl_shift_up_no_inp(ovec!["val = { camelCase123┃ }"], ovec!["val = ┃❮{ camelCase123 }❯"])?;*/

        assert_ctrl_shift_up_no_inp(ovec!["val = { a: \"┃\" }"], ovec!["val = { a: ┃❮\"\"❯ }"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a: ┃\"\" }"], ovec!["val = { a: ┃❮\"\"❯ }"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a: \"\"┃ }"], ovec!["val = ┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a: \"\" ┃}"], ovec!["val = ┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { a: \"\" ┃}"],
            ovec!["┃❮val = { a: \"\" }❯"],
            3,
        )?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a: \"\" }┃"], ovec!["val = ┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a:┃ \"\" }"], ovec!["val = ┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a┃: \"\" }"], ovec!["val = ┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { ┃a: \"\" }"], ovec!["val = ┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = {┃ a: \"\" }"], ovec!["val = ┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = ┃{ a: \"\" }"], ovec!["val = ┃❮{ a: \"\" }❯"])?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { a: \"┃\" }"],
            ovec!["val = ┃❮{ a: \"\" }❯"],
            2,
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { a: \"┃\" }"],
            ovec!["┃❮val = { a: \"\" }❯"],
            4,
        )?;

        assert_ctrl_shift_up_no_inp(ovec!["val = { a: 1┃0 }"], ovec!["val = { a: ┃❮10❯ }"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a: ┃9 }"], ovec!["val = { a: ┃❮9❯ }"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a: 98┃89 }"], ovec!["val = { a: ┃❮9889❯ }"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a: 44┃ }"], ovec!["val = ┃❮{ a: 44 }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a: 0 ┃}"], ovec!["val = ┃❮{ a: 0 }❯"])?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { a: 123 ┃}"],
            ovec!["┃❮val = { a: 123 }❯"],
            3,
        )?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a: 96 }┃"], ovec!["val = ┃❮{ a: 96 }❯"])?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { a:┃ 985600 }"],
            ovec!["val = ┃❮{ a: 985600 }❯"],
        )?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { a┃: 5648 }"], ovec!["val = ┃❮{ a: 5648 }❯"])?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { ┃a: 1000000 }"],
            ovec!["val = ┃❮{ a: 1000000 }❯"],
        )?;
        assert_ctrl_shift_up_no_inp(ovec!["val = {┃ a: 1 }"], ovec!["val = ┃❮{ a: 1 }❯"])?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = ┃{ a: 900600 }"],
            ovec!["val = ┃❮{ a: 900600 }❯"],
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { a: 10┃000 }"],
            ovec!["val = ┃❮{ a: 10000 }❯"],
            2,
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { a: ┃45 }"],
            ovec!["┃❮val = { a: 45 }❯"],
            4,
        )?;

        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: \"de┃\" }"],
            ovec!["val = { abc: ┃❮\"de\"❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: \"d┃e\" }"],
            ovec!["val = { abc: ┃❮\"de\"❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: \"┃de\" }"],
            ovec!["val = { abc: ┃❮\"de\"❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: ┃\"de\" }"],
            ovec!["val = { abc: ┃❮\"de\"❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: \"de\"┃ }"],
            ovec!["val = ┃❮{ abc: \"de\" }❯"],
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { abc: \"d┃e\" }"],
            ovec!["val = ┃❮{ abc: \"de\" }❯"],
            2,
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { abc: \"d┃e\" }"],
            ovec!["┃❮val = { abc: \"de\" }❯"],
            3,
        )?;

        assert_ctrl_shift_up_no_inp(
            ovec!["val = { camelCase123: \"hello, hello.012┃3456789ZXY{}[]-><-\" }"],
            ovec!["val = { camelCase123: ┃❮\"hello, hello.0123456789ZXY{}[]-><-\"❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { camel┃Case123: \"hello, hello.0123456789ZXY{}[]-><-\" }"],
            ovec!["val = ┃❮{ camelCase123: \"hello, hello.0123456789ZXY{}[]-><-\" }❯"],
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { camelCase123: \"hello, hello┃.0123456789ZXY{}[]-><-\" }"],
            ovec!["val = ┃❮{ camelCase123: \"hello, hello.0123456789ZXY{}[]-><-\" }❯"],
            2,
        )?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_nested_record() -> Result<(), String> {
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { ┃ } }"],
            ovec!["val = { abc: ┃❮{  }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: {┃  } }"],
            ovec!["val = { abc: ┃❮{  }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: ┃{  } }"],
            ovec!["val = { abc: ┃❮{  }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: {  ┃} }"],
            ovec!["val = { abc: ┃❮{  }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: {  }┃ }"],
            ovec!["val = ┃❮{ abc: {  } }❯"],
        )?;

        // TODO uncomment tests once #1649 is fixed
        /*assert_ctrl_shift_up_no_inp(ovec!["val = { abc: { ┃d } }"], ovec!["val = { abc: ┃❮{ d }❯ }"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { abc: {┃ d } }"], ovec!["val = { abc: ┃❮{ d }❯ }"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { abc: ┃{ d } }"], ovec!["val = { abc: ┃❮{ d }❯ }"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { abc: { d ┃} }"], ovec!["val = { abc: ┃❮{ d }❯ }"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { abc: { d┃e } }"], ovec!["val = { abc: ┃❮{ de }❯ }"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = { abc: { d }┃ }"], ovec!["val = ┃❮{ abc: { d } }❯"])?;
        assert_ctrl_shift_up_no_inp(ovec!["val = ┃{ abc: { d } }"], ovec!["val = ┃❮{ abc: { d } }❯"])?;*/

        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: { ┃ } } }"],
            ovec!["val = { abc: { de: ┃❮{  }❯ } }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: ┃{  } } }"],
            ovec!["val = { abc: { de: ┃❮{  }❯ } }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: {  }┃ } }"],
            ovec!["val = { abc: ┃❮{ de: {  } }❯ }"],
        )?;

        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: \"┃\" } }"],
            ovec!["val = { abc: { de: ┃❮\"\"❯ } }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: ┃\"\" } }"],
            ovec!["val = { abc: { de: ┃❮\"\"❯ } }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: \"\"┃ } }"],
            ovec!["val = { abc: ┃❮{ de: \"\" }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: \"f g┃\" } }"],
            ovec!["val = { abc: { de: ┃❮\"f g\"❯ } }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de┃: \"f g\" } }"],
            ovec!["val = { abc: ┃❮{ de: \"f g\" }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: {┃ de: \"f g\" } }"],
            ovec!["val = { abc: ┃❮{ de: \"f g\" }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: \"f g\" ┃} }"],
            ovec!["val = { abc: ┃❮{ de: \"f g\" }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: \"f g\" }┃ }"],
            ovec!["val = ┃❮{ abc: { de: \"f g\" } }❯"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = ┃{ abc: { de: \"f g\" } }"],
            ovec!["val = ┃❮{ abc: { de: \"f g\" } }❯"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: \"f g\" } }┃"],
            ovec!["val = ┃❮{ abc: { de: \"f g\" } }❯"],
        )?;

        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { abc: { de: \"f g┃\" } }"],
            ovec!["val = { abc: ┃❮{ de: \"f g\" }❯ }"],
            2,
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { abc: { de: ┃\"f g\" } }"],
            ovec!["val = ┃❮{ abc: { de: \"f g\" } }❯"],
            3,
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { abc: { de: ┃\"f g\" } }"],
            ovec!["┃❮val = { abc: { de: \"f g\" } }❯"],
            4,
        )?;

        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: ┃951 } }"],
            ovec!["val = { abc: { de: ┃❮951❯ } }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: 11┃0 } }"],
            ovec!["val = { abc: { de: ┃❮110❯ } }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: 444┃ } }"],
            ovec!["val = { abc: ┃❮{ de: 444 }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de┃: 99 } }"],
            ovec!["val = { abc: ┃❮{ de: 99 }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: {┃ de: 0 } }"],
            ovec!["val = { abc: ┃❮{ de: 0 }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: 230 ┃} }"],
            ovec!["val = { abc: ┃❮{ de: 230 }❯ }"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: 7 }┃ }"],
            ovec!["val = ┃❮{ abc: { de: 7 } }❯"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = ┃{ abc: { de: 1 } }"],
            ovec!["val = ┃❮{ abc: { de: 1 } }❯"],
        )?;
        assert_ctrl_shift_up_no_inp(
            ovec!["val = { abc: { de: 111111 } }┃"],
            ovec!["val = ┃❮{ abc: { de: 111111 } }❯"],
        )?;

        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { abc: { de: 1┃5 } }"],
            ovec!["val = { abc: ┃❮{ de: 15 }❯ }"],
            2,
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { abc: { de: ┃55 } }"],
            ovec!["val = ┃❮{ abc: { de: 55 } }❯"],
            3,
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { abc: { de: ┃400 } }"],
            ovec!["┃❮val = { abc: { de: 400 } }❯"],
            5,
        )?;

        // TODO uncomment tests once #1649 is fixed
        /*assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { g: { oi: { ng: { d: { e: { e: { p: { camelCase┃ } } } } } } } }"],
            ovec!["val = { g: { oi: { ng: { d: ┃❮{ e: { e: { p: { camelCase } } } }❯ } } } }"],
            4,
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { g: { oi: { ng: { d: { e: { e: { p: { camelCase┃ } } } } } } } }"],
            ovec!["val = { g: ┃❮{ oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } }❯ }"],
            7,
        )?;
        assert_ctrl_shift_up_repeat_no_inp(
            ovec!["val = { g: { oi: { ng: { d: { e: { e: { p: { camelCase┃ } } } } } } } }"],
            ovec!["val = ┃❮{ g: { oi: { ng: { d: { e: { e: { p: { camelCase } } } } } } } }❯"],
            9,
        )?;*/

        Ok(())
    }

    // Create ed_model from pre_lines DSL, do handle_new_char() with new_char_seq, select current Expr2,
    // check if generated tooltips match expected_tooltips.
    pub fn assert_type_tooltips_seq(
        pre_lines: Vec<String>,
        expected_tooltips: Vec<String>,
        new_char_seq: &str,
    ) -> Result<(), String> {
        let mut code_str = pre_lines.join("").replace('┃', "");

        let mut model_refs = init_model_refs();
        let code_arena = Bump::new();
        let module_ids = ModuleIds::default();

        let mut ed_model = ed_model_from_dsl(
            &mut code_str,
            pre_lines,
            &mut model_refs,
            &module_ids,
            &code_arena,
        )?;

        for input_char in new_char_seq.chars() {
            if input_char == '🡲' {
                ed_model.simple_move_carets_right(1);
            } else {
                ed_res_to_res(handle_new_char(&input_char, &mut ed_model))?;
            }
        }

        for expected_tooltip in expected_tooltips.iter() {
            ed_model.select_expr()?;

            let created_tooltip = ed_model.selected_block_opt.unwrap().type_str;

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
        pre_lines: Vec<String>,
        expected_tooltip: &str,
        new_char: char,
    ) -> Result<(), String> {
        assert_type_tooltips_seq(pre_lines, ovec![expected_tooltip], &new_char.to_string())
    }

    pub fn assert_type_tooltip_clean(
        lines: Vec<String>,
        expected_tooltip: &str,
    ) -> Result<(), String> {
        assert_type_tooltips_seq(lines, ovec![expected_tooltip], "")
    }

    // When doing ctrl+shift+up multiple times we select the surrounding expression every time,
    // every new selection should have the correct tooltip
    pub fn assert_type_tooltips_clean(
        lines: Vec<String>,
        expected_tooltips: Vec<String>,
    ) -> Result<(), String> {
        assert_type_tooltips_seq(lines, expected_tooltips, "")
    }

    #[test]
    fn test_type_tooltip() -> Result<(), String> {
        assert_type_tooltip_clean(ovec!["val = ┃5"], "Num *")?;
        assert_type_tooltip_clean(ovec!["val = 42┃"], "Num *")?;
        assert_type_tooltip_clean(ovec!["val = 13┃7"], "Num *")?;

        assert_type_tooltip_clean(ovec!["val = \"┃abc\""], "Str")?;
        assert_type_tooltip_clean(ovec!["val = ┃\"abc\""], "Str")?;
        assert_type_tooltip_clean(ovec!["val = \"abc\"┃"], "Str")?;

        assert_type_tooltip_clean(ovec!["val = { ┃ }"], "{}")?;
        assert_type_tooltip_clean(ovec!["val = { a: \"abc\" }┃"], "{ a : Str }")?;
        assert_type_tooltip_clean(ovec!["val = { ┃a: 0 }"], "{ a : Num * }")?;
        assert_type_tooltip_clean(ovec!["val = { ┃z: {  } }"], "{ z : {} }")?;
        assert_type_tooltip_clean(ovec!["val = { camelCase: ┃0 }"], "Num *")?;

        assert_type_tooltips_seq(ovec!["┃"], ovec!["*"], "val=🡲🡲🡲")?;
        assert_type_tooltips_seq(
            ovec!["┃"],
            ovec!["*", "{ a : * }", "{ a : * }"],
            "val=🡲🡲🡲{a:",
        )?;

        assert_type_tooltips_clean(
            ovec!["val = { camelCase: ┃0 }"],
            ovec!["Num *", "{ camelCase : Num * }"],
        )?;
        assert_type_tooltips_clean(
            ovec!["val = { a: { b: { c: \"hello┃, hello.0123456789ZXY{}[]-><-\" } } }"],
            ovec![
                "Str",
                "{ c : Str }",
                "{ b : { c : Str } }",
                "{ a : { b : { c : Str } } }"
            ],
        )?;

        Ok(())
    }

    #[test]
    fn test_type_tooltip_list() -> Result<(), String> {
        assert_type_tooltip_clean(ovec!["val = [ ┃ ]"], "List *")?;
        assert_type_tooltips_clean(ovec!["val = [ ┃0 ]"], ovec!["Num *", "List (Num *)"])?;
        assert_type_tooltips_clean(
            ovec!["val = [ [ ┃0 ] ]"],
            ovec!["Num *", "List (Num *)", "List (List (Num *))"],
        )?;

        assert_type_tooltips_clean(
            ovec!["val = [ [ [ \"ab┃c\" ] ] ]"],
            ovec![
                "Str",
                "List Str",
                "List (List Str)",
                "List (List (List Str))"
            ],
        )?;
        assert_type_tooltips_clean(
            ovec!["val = [ [ { a┃: 1 } ] ]"],
            ovec![
                "{ a : Num * }",
                "List { a : Num * }",
                "List (List { a : Num * })"
            ],
        )?;

        // multi element lists
        assert_type_tooltips_clean(ovec!["val = [ ┃1, 2, 3 ]"], ovec!["Num *", "List (Num *)"])?;
        assert_type_tooltips_clean(
            ovec!["val = [ \"┃abc\", \"de\", \"f\" ]"],
            ovec!["Str", "List Str"],
        )?;
        assert_type_tooltips_clean(
            ovec!["val = [ { a:┃ 1 }, { a: 12 }, { a: 444 } ]"],
            ovec!["{ a : Num * }", "List { a : Num * }"],
        )?;

        Ok(())
    }

    #[test]
    fn test_type_tooltip_mismatch() -> Result<(), String> {
        assert_type_tooltips_clean(
            ovec!["val = [ 1, \"ab┃c\" ]"],
            ovec!["Str", "List <type mismatch>"],
        )?;
        assert_type_tooltips_clean(
            ovec!["val = [ \"abc\", 5┃0 ]"],
            ovec!["Num *", "List <type mismatch>"],
        )?;

        assert_type_tooltips_clean(
            ovec!["val = [ { a: 0 }, { a: \"0┃\" } ]"],
            ovec!["Str", "{ a : Str }", "List <type mismatch>"],
        )?;

        assert_type_tooltips_clean(
            ovec!["val = [ [ 0, 1, \"2\" ], [ 3, 4, 5 ┃] ]"],
            ovec!["List (Num *)", "List (List <type mismatch>)"],
        )?;

        Ok(())
    }

    type ModelMoveCaretFun = fn(&mut EdModel<'_>, &Modifiers) -> UIResult<()>;

    // Create ed_model from pre_lines DSL, do ctrl+shift+up as many times as repeat. Then move the caret by executing
    // move_caret_fun. Next check if modified ed_model has expected string representation of code, caret position and
    // active selection.
    fn assert_ctrl_shift_up_move(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        repeats: usize,
        move_caret_fun: ModelMoveCaretFun,
    ) -> Result<(), String> {
        let mut code_str = pre_lines.join("").replace('┃', "");

        let mut model_refs = init_model_refs();
        let code_arena = Bump::new();
        let module_ids = ModuleIds::default();

        let mut ed_model = ed_model_from_dsl(
            &mut code_str,
            pre_lines,
            &mut model_refs,
            &module_ids,
            &code_arena,
        )?;

        for _ in 0..repeats {
            ed_model.ed_handle_key_down(&ctrl_cmd_shift(), Up, &mut ThreadPool::new(1))?;
        }

        move_caret_fun(&mut ed_model, &no_mods())?;

        let mut post_lines = ui_res_to_res(ed_model_to_dsl(&ed_model))?;
        strip_header(&mut post_lines); // remove header for clean tests

        assert_eq!(post_lines, expected_post_lines);

        Ok(())
    }

    fn assert_ctrl_shift_up_move_nls(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        repeats: usize,
        move_caret_fun: ModelMoveCaretFun,
    ) -> Result<(), String> {
        assert_ctrl_shift_up_move(
            pre_lines,
            add_nls(expected_post_lines),
            repeats,
            move_caret_fun,
        )
    }

    fn assert_ctrl_shift_single_up_move(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        move_caret_fun: ModelMoveCaretFun,
    ) -> Result<(), String> {
        assert_ctrl_shift_up_move(pre_lines, expected_post_lines, 1, move_caret_fun)
    }

    fn assert_ctrl_shift_single_up_move_nls(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        move_caret_fun: ModelMoveCaretFun,
    ) -> Result<(), String> {
        assert_ctrl_shift_single_up_move(pre_lines, add_nls(expected_post_lines), move_caret_fun)
    }

    // because complex lifetime stuff
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
    fn test_ctrl_shift_up_move_int() -> Result<(), String> {
        assert_ctrl_shift_single_up_move_nls(ovec!["val = ┃0"], ovec!["val = 0┃"], move_down!())?;
        assert_ctrl_shift_single_up_move_nls(
            ovec!["val = ┃9654"],
            ovec!["val = ┃9654"],
            move_up!(),
        )?;
        assert_ctrl_shift_single_up_move_nls(
            ovec!["val = ┃100546"],
            ovec!["val = 100546┃"],
            move_end!(),
        )?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_move_string() -> Result<(), String> {
        assert_ctrl_shift_single_up_move_nls(
            ovec!["val = ┃\"\""],
            ovec!["val = \"\"┃"],
            move_down!(),
        )?;
        assert_ctrl_shift_single_up_move_nls(
            ovec!["val = ┃\"abc\""],
            ovec!["val = ┃\"abc\""],
            move_up!(),
        )?;
        assert_ctrl_shift_single_up_move_nls(
            ovec!["val = ┃\"hello, hello.0123456789ZXY{}[]-><-\""],
            ovec!["val = \"hello, hello.0123456789ZXY{}[]-><-\"┃"],
            move_end!(),
        )?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_move_record() -> Result<(), String> {
        assert_ctrl_shift_single_up_move_nls(
            ovec!["val = ┃{  }"],
            ovec!["┃val = {  }"],
            move_home!(),
        )?;
        // TODO uncomment tests once #1649 is fixed.
        //assert_ctrl_shift_single_up_move(ovec!["┃{ a }"], ovec!["{ a }┃"], move_down!())?;
        //assert_ctrl_shift_single_up_move(ovec!["┃{ a: { b } }"], ovec!["{ a: { b } }┃"], move_right!())?;
        assert_ctrl_shift_single_up_move_nls(
            ovec!["val = { a: { ┃ } }"],
            ovec!["val = { a: {  } }┃"],
            move_end!(),
        )?;
        assert_ctrl_shift_up_move_nls(
            ovec!["val = { a: { b: { ┃ } } }"],
            ovec!["val = { a: ┃{ b: {  } } }"],
            2,
            move_up!(),
        )?;
        assert_ctrl_shift_up_move_nls(
            ovec!["val = { camelCase: { cC123: \"hello┃, hello.0123456789ZXY{}[]-><-\" } }"],
            ovec!["val = { camelCase: { cC123: \"hello, hello.0123456789ZXY{}[]-><-\" }┃ }"],
            2,
            move_down!(),
        )?;

        assert_ctrl_shift_up_move_nls(
            ovec!["val = { camelCase: { cC123: 9┃5 } }"],
            ovec!["val = { camelCase: { cC123: 95 }┃ }"],
            2,
            move_down!(),
        )?;

        Ok(())
    }

    // Create ed_model from pre_lines DSL, do ctrl+shift+up as many times as repeat. Then do backspace.
    // Next check if modified ed_model has expected string representation of code, caret position and
    // active selection.
    fn assert_ctrl_shift_up_backspace(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        repeats: usize,
    ) -> Result<(), String> {
        let mut code_str = pre_lines.join("").replace('┃', "");

        let mut model_refs = init_model_refs();
        let code_arena = Bump::new();
        let module_ids = ModuleIds::default();

        let mut ed_model = ed_model_from_dsl(
            &mut code_str,
            pre_lines,
            &mut model_refs,
            &module_ids,
            &code_arena,
        )?;

        for _ in 0..repeats {
            ed_model.ed_handle_key_down(&ctrl_cmd_shift(), Up, &mut ThreadPool::new(1))?;
        }

        handle_new_char(&'\u{8}', &mut ed_model)?; // \u{8} is the char for backspace on linux

        let mut post_lines = ui_res_to_res(ed_model_to_dsl(&ed_model))?;
        strip_header(&mut post_lines);

        assert_eq!(post_lines, expected_post_lines);

        Ok(())
    }
    fn assert_ctrl_shift_up_backspace_nls(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
        repeats: usize,
    ) -> Result<(), String> {
        assert_ctrl_shift_up_backspace(pre_lines, add_nls(expected_post_lines), repeats)
    }

    fn assert_ctrl_shift_single_up_backspace(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
    ) -> Result<(), String> {
        assert_ctrl_shift_up_backspace(pre_lines, expected_post_lines, 1)
    }

    fn assert_ctrl_shift_single_up_backspace_nls(
        pre_lines: Vec<String>,
        expected_post_lines: Vec<String>,
    ) -> Result<(), String> {
        assert_ctrl_shift_single_up_backspace(pre_lines, add_nls(expected_post_lines))
    }

    #[test]
    fn test_ctrl_shift_up_backspace_int() -> Result<(), String> {
        // Blank is inserted when root is deleted
        assert_ctrl_shift_single_up_backspace_nls(ovec!["val = 95┃21"], ovec!["val = ┃ "])?;
        assert_ctrl_shift_single_up_backspace_nls(ovec!["val = 0┃"], ovec!["val = ┃ "])?;
        assert_ctrl_shift_single_up_backspace_nls(ovec!["val = ┃10000"], ovec!["val = ┃ "])?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_backspace_string() -> Result<(), String> {
        // Blank is inserted when root is deleted
        assert_ctrl_shift_single_up_backspace_nls(ovec!["val = \"┃\""], ovec!["val = ┃ "])?;
        assert_ctrl_shift_single_up_backspace_nls(ovec!["val = \"\"┃"], ovec!["val = ┃ "])?;
        assert_ctrl_shift_single_up_backspace_nls(ovec!["val = ┃\"abc\""], ovec!["val = ┃ "])?;
        assert_ctrl_shift_single_up_backspace_nls(
            ovec!["val = \"hello┃, hello.0123456789ZXY{}[]-><-\""],
            ovec!["val = ┃ "],
        )?;

        Ok(())
    }

    #[test]
    fn test_ctrl_shift_up_backspace_record() -> Result<(), String> {
        // Blank is inserted when root of Expr2 is deleted
        assert_ctrl_shift_single_up_backspace_nls(ovec!["val = {┃  }"], ovec!["val = ┃ "])?;

        // TODO: uncomment tests, once issue #1649 is fixed
        //assert_ctrl_shift_single_up_backspace(ovec!["{ a┃ }"], ovec!["┃ "])?;
        //assert_ctrl_shift_single_up_backspace(ovec!["{ a: { b }┃ }"], ovec!["┃ "])?;
        assert_ctrl_shift_single_up_backspace_nls(
            ovec!["val = { a: \"b cd\"┃ }"],
            ovec!["val = ┃ "],
        )?;

        //assert_ctrl_shift_single_up_backspace(ovec!["{ a: ┃{ b } }"], ovec!["{ a: ┃  }"])?;
        assert_ctrl_shift_single_up_backspace_nls(
            ovec!["val = { a: \"┃b cd\" }"],
            ovec!["val = { a: ┃  }"],
        )?;
        assert_ctrl_shift_single_up_backspace_nls(
            ovec!["val = { a: ┃12 }"],
            ovec!["val = { a: ┃  }"],
        )?;
        /*assert_ctrl_shift_single_up_backspace(
            ovec!["{ g: { oi: { ng: { d: { ┃e: { e: { p: { camelCase } } } } } } } }"],
            ovec!["{ g: { oi: { ng: { d: ┃  } } } }"],
        )?;*/

        assert_ctrl_shift_up_backspace_nls(
            ovec!["val = { a: { b: { c: \"abc┃  \" } } }"],
            ovec!["val = { a: { b: ┃  } }"],
            2,
        )?;
        assert_ctrl_shift_up_backspace_nls(
            ovec!["val = { a: { b: { c: 100┃000 } } }"],
            ovec!["val = { a: { b: ┃  } }"],
            2,
        )?;
        assert_ctrl_shift_up_backspace_nls(
            ovec!["val = { a: { b: { c: {┃  } } } }"],
            ovec!["val = { a: { b: ┃  } }"],
            2,
        )?;
        /*assert_ctrl_shift_up_backspace(
            ovec!["{ g: { oi: { ng: { d: { e: { e: { p┃: { camelCase } } } } } } } }"],
            ovec!["{ g: ┃  }"],
            6,
        )?;*/

        Ok(())
    }
}
