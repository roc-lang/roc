use crate::editor::ed_error::EdResult;
use crate::graphics::primitives::rect::Rect;
use crate::ui::text::{
    big_selectable_text::{BigSelectableText, from_path},
    lines::{SelectableLines, MutSelectableLines},
    text_pos::TextPos,
    selection::Selection,
};
use crate::ui::ui_error::UIResult;
use std::path::Path;

#[derive(Debug)]
pub struct EdModel {
    pub text: BigSelectableText,
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool,
}

pub fn init_model(file_path: &Path) -> UIResult<EdModel> {
    Ok(EdModel {
        text: from_path(file_path)?,
        glyph_dim_rect_opt: None,
        has_focus: true,
    })
}
