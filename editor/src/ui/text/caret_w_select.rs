#![allow(dead_code)]

use super::selection::validate_selection;
use super::selection::Selection;
use super::text_pos::TextPos;
use crate::ui::ui_error::UIResult;
use crate::window::keyboard_input::Modifiers;

#[derive(Debug, Copy, Clone)]
pub struct CaretWSelect {
    pub caret_pos: TextPos,
    pub selection_opt: Option<Selection>,
}

fn mk_some_sel(start_pos: TextPos, end_pos: TextPos) -> UIResult<Option<Selection>> {
    if start_pos == end_pos {
        Ok(None)
    } else {
        Ok(Some(validate_selection(start_pos, end_pos)?))
    }
}

impl Default for CaretWSelect {
    fn default() -> Self {
        Self {
            caret_pos: TextPos { line: 0, column: 0 },
            selection_opt: None,
        }
    }
}

impl CaretWSelect {
    pub fn new(caret_pos: TextPos, selection_opt: Option<Selection>) -> Self {
        Self {
            caret_pos,
            selection_opt,
        }
    }

    pub fn move_caret_w_mods(&self, new_pos: TextPos, mods: &Modifiers) -> UIResult<CaretWSelect> {
        let old_caret_pos = self.caret_pos;

        // one does not simply move the caret
        let valid_sel_opt = if mods.shift {
            if new_pos != old_caret_pos {
                if let Some(old_sel) = self.selection_opt {
                    if new_pos < old_sel.start_pos {
                        if old_caret_pos > old_sel.start_pos {
                            mk_some_sel(new_pos, old_sel.start_pos)?
                        } else {
                            mk_some_sel(new_pos, old_sel.end_pos)?
                        }
                    } else if new_pos > old_sel.end_pos {
                        if old_caret_pos < old_sel.end_pos {
                            mk_some_sel(old_sel.end_pos, new_pos)?
                        } else {
                            mk_some_sel(old_sel.start_pos, new_pos)?
                        }
                    } else if new_pos > old_caret_pos {
                        mk_some_sel(new_pos, old_sel.end_pos)?
                    } else if new_pos < old_caret_pos {
                        mk_some_sel(old_sel.start_pos, new_pos)?
                    } else {
                        None
                    }
                } else if new_pos < self.caret_pos {
                    mk_some_sel(new_pos, old_caret_pos)?
                } else {
                    mk_some_sel(old_caret_pos, new_pos)?
                }
            } else {
                self.selection_opt
            }
        } else {
            None
        };

        Ok(CaretWSelect::new(new_pos, valid_sel_opt))
    }
}

// VIEW
// ----
use crate::graphics::primitives::rect::Rect;
use crate::ui::theme::UITheme;

pub fn make_caret_rect_from_pos(
    caret_pos: TextPos,
    glyph_dim_rect: &Rect,
    ui_theme: &UITheme,
) -> Rect {
    let caret_x =
        glyph_dim_rect.top_left_coords.x + glyph_dim_rect.width * (caret_pos.column as f32);

    let caret_y =
        glyph_dim_rect.top_left_coords.y + (caret_pos.line as f32) * glyph_dim_rect.height;

    make_caret_rect(caret_x, caret_y, glyph_dim_rect, ui_theme)
}

pub fn make_caret_rect(
    caret_x: f32,
    caret_y: f32,
    glyph_dim_rect: &Rect,
    ui_theme: &UITheme,
) -> Rect {
    Rect {
        top_left_coords: (caret_x, caret_y).into(),
        height: glyph_dim_rect.height,
        width: 2.0,
        color: ui_theme.caret,
    }
}
