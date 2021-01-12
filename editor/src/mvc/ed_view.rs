
use super::ed_model::{EdModel, Position};
use crate::graphics::primitives::rect::Rect;
use crate::error::{EdResult, MissingGlyphDims};
use crate::selection::create_selection_rects;
use crate::graphics::colors::{CARET_COLOR};
use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use snafu::{ensure};

//TODO add editor text here as well

pub fn create_ed_rects<'a>(ed_model: &EdModel, arena: &'a Bump) -> EdResult<BumpVec<'a, Rect>> {
    ensure!(ed_model.glyph_dim_rect_opt.is_some(),
        MissingGlyphDims {}
    );

    let glyph_rect = ed_model.glyph_dim_rect_opt.unwrap();

    let mut all_rects: BumpVec<Rect> = BumpVec::new_in(arena);

    if let Some(selection) = ed_model.selection_opt {
        let mut selection_rects =
            create_selection_rects(selection, &ed_model.text_buf, &glyph_rect, &arena)?;

        all_rects.append(&mut selection_rects);
    }

    all_rects.push(make_caret_rect(ed_model.caret_pos, &glyph_rect));

    Ok(all_rects)
}

fn make_caret_rect(caret_pos: Position, glyph_dim_rect: &Rect) -> Rect {
    let caret_y =
        glyph_dim_rect.top_left_coords.y + (caret_pos.line as f32) * glyph_dim_rect.height;

    let caret_x =
        glyph_dim_rect.top_left_coords.x + glyph_dim_rect.width * (caret_pos.column as f32);

    Rect {
        top_left_coords: (caret_x, caret_y).into(),
        height: glyph_dim_rect.height,
        width: 2.0,
        color: CARET_COLOR,
    }
}
