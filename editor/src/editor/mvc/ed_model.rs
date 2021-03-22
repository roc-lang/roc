use std::path::Path;
use crate::ui::text::selection::validate_raw_sel;
use crate::ui::text::selection::RawSelection;
use crate::ui::text::selection::Selection;
use crate::ui::ui_error::UIResult;
use crate::ui::text::lines::MoveCaretFun;
use crate::editor::code_lines::CodeLines;
use crate::ui::text::text_pos::TextPos;
use crate::ui::text::{
    lines,
    lines::Lines,
    lines::SelectableLines,
};
use crate::ui::text::caret_w_select::CaretWSelect;
use crate::editor::slow_pool::{SlowNodeId, SlowPool};
use crate::editor::syntax_highlight::HighlightStyle;
use crate::editor::{
    ed_error::EdError::ParseError,
    ed_error::EdResult,
    markup::attribute::{Attributes, Caret},
    markup::nodes::{expr2_to_markup, set_parent_for_all, MarkupNode},
};
use crate::graphics::primitives::rect::Rect;
use crate::lang::ast::Expr2;
use crate::lang::expr::{str_to_expr2, Env};
use crate::lang::scope::Scope;
use crate::window::keyboard_input::Modifiers;
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use roc_region::all::Region;
use winit::event::VirtualKeyCode;
use nonempty::NonEmpty;

#[derive(Debug)]
pub struct EdModel<'a> {
    pub module: EdModule<'a>,
    pub code_lines: CodeLines,
    pub markup_root_id: SlowNodeId,
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool,
    caret_w_select_vec:  NonEmpty<(CaretWSelect, Option<SlowNodeId>)>,
    // Option<SlowNodeId>: MarkupNode that corresponds to caret position, Option because this SlowNodeId is only calculated when it needs to be used.
}

pub fn init_model<'a>(
    code_str: &'a BumpString,
    file_path: &'a Path,
    env: Env<'a>,
    code_arena: &'a Bump,
    markup_node_pool: &mut SlowPool,
) -> EdResult<EdModel<'a>> {
    let mut module = EdModule::new(&code_str, env, code_arena)?;
    // TODO fix moving issue and insert module.ast_root into pool
    let ast_root_id = module.env.pool.add(Expr2::Blank);

    let markup_root_id = if code_str.is_empty() {
        let blank_root = MarkupNode::Blank {
            ast_node_id: ast_root_id,
            attributes: Attributes {
                all: vec![Caret::new_attr(0)],
            },
            syn_high_style: HighlightStyle::Blank,
            parent_id_opt: None,
        };

        markup_node_pool.add(blank_root)
    } else {
        let temp_markup_root_id = expr2_to_markup(
            code_arena,
            &mut module.env,
            &module.ast_root,
            markup_node_pool,
        );
        set_parent_for_all(temp_markup_root_id, markup_node_pool);

        temp_markup_root_id
    };

    Ok(EdModel {
        module,
        code_lines: CodeLines::from_bump_str(code_str),
        markup_root_id,
        glyph_dim_rect_opt: None,
        has_focus: true,
        caret_w_select_vec: NonEmpty::new((CaretWSelect::default(), None)),
    })
}

impl<'a> EdModel<'a> {
    pub fn handle_key_down(
        &mut self,
        modifiers: &Modifiers,
        virtual_keycode: VirtualKeyCode,
        markup_node_pool: &mut SlowPool,
    ) -> EdResult<()> {
        match virtual_keycode {
            VirtualKeyCode::Right => {
                self.move_caret_right(modifiers)?;
            }
            VirtualKeyCode::Left => {
                self.move_caret_left(modifiers)?;
            },
            _ => (),
        };

        Ok(())
    }

    pub fn move_caret(&mut self, move_fun: MoveCaretFun<CodeLines>, modifiers: &Modifiers) -> UIResult<()> {
        for caret_tup in self.caret_w_select_vec.iter_mut() {
            caret_tup.0 = move_fun(&self.code_lines, caret_tup.0, modifiers)?;
            caret_tup.1 = None;
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

                Ok(
                    Some(
                        line_ref[start_col..end_col].to_string()                    )
                )
            } else {
                let full_str = String::new();

                // TODO
                Ok(
                    Some(
                        full_str
                    )
                )
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

        Ok(
            TextPos {
                line: self.code_lines.lines.len() - 1,
                column: last_line.len(),
            }
        )
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

#[derive(Debug)]
pub struct EdModule<'a> {
    pub env: Env<'a>,
    pub ast_root: Expr2,
}

impl<'a> EdModule<'a> {
    pub fn new(code_str: &'a str, mut env: Env<'a>, ast_arena: &'a Bump) -> EdResult<EdModule<'a>> {
        if !code_str.is_empty() {
            let mut scope = Scope::new(env.home, env.pool, env.var_store);

            let region = Region::new(0, 0, 0, 0);

            let expr2_result = str_to_expr2(&ast_arena, &code_str, &mut env, &mut scope, region);

            match expr2_result {
                Ok((expr2, _output)) => Ok(EdModule {
                    env,
                    ast_root: expr2,
                }),
                Err(err) => Err(ParseError {
                    syntax_err: format!("{:?}", err),
                }),
            }
        } else {
            Ok(EdModule {
                env,
                ast_root: Expr2::Blank,
            })
        }
    }
}
