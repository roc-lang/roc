/*use super::ed_model::EdModel;
use crate::graphics::primitives::rect::Rect;
use crate::ui::{
    text::{selection::create_selection_rects, text_pos::TextPos},
    theme::UITheme,
    ui_error::{MissingGlyphDims, UIResult},
};

use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use snafu::ensure;

//TODO add editor text here as well

pub fn create_ed_rects<'a>(
    ed_model: &EdModel,
    ui_theme: &UITheme,
    arena: &'a Bump,
) -> UIResult<BumpVec<'a, Rect>> {
    ensure!(ed_model.glyph_dim_rect_opt.is_some(), MissingGlyphDims {});

    let glyph_rect = ed_model.glyph_dim_rect_opt.unwrap();

    let mut all_rects: BumpVec<Rect> = BumpVec::new_in(arena);

    let selection_opt = ed_model.text.caret_w_select.selection_opt;

    if let Some(selection) = selection_opt {
        let mut selection_rects =
            create_selection_rects(selection, &ed_model.text, &glyph_rect, ui_theme, &arena)?;

        all_rects.append(&mut selection_rects);
    }

    let caret_pos = ed_model.text.caret_w_select.caret_pos;
    all_rects.push(make_caret_rect(caret_pos, &glyph_rect, ui_theme));

    Ok(all_rects)
}*/
