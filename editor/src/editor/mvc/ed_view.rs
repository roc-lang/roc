use super::ed_model::EdModel;
use crate::graphics::primitives::rect::Rect;
use crate::ui::{
    theme::UITheme,
    text::{selection::create_selection_rects, text_pos::TextPos},
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
}

fn make_caret_rect(caret_pos: TextPos, glyph_dim_rect: &Rect, ui_theme: &UITheme) -> Rect {
    let caret_y =
        glyph_dim_rect.top_left_coords.y + (caret_pos.line as f32) * glyph_dim_rect.height;

    let caret_x =
        glyph_dim_rect.top_left_coords.x + glyph_dim_rect.width * (caret_pos.column as f32);

    Rect {
        top_left_coords: (caret_x, caret_y).into(),
        height: glyph_dim_rect.height,
        width: 2.0,
        color: ui_theme.caret,
    }
}
