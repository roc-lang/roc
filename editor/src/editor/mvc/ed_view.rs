use super::ed_model::EdModel;
use crate::editor::config::Config;
use crate::editor::ed_error::EdResult;
use crate::editor::render_ast::build_code_graphics;
use crate::editor::render_debug::build_debug_graphics;
use crate::graphics::primitives::rect::Rect;
use crate::ui::text::caret_w_select::make_caret_rect;
use crate::ui::text::caret_w_select::CaretWSelect;
use crate::ui::ui_error::MissingGlyphDims;
use cgmath::Vector2;
use snafu::OptionExt;
use winit::dpi::PhysicalSize;

#[derive(Debug)]
pub struct RenderedWgpu {
    pub text_sections: Vec<glyph_brush::OwnedSection>,
    pub rects: Vec<Rect>,
}

// create text and rectangles based on EdModel's markup_root
pub fn model_to_wgpu<'a>(
    ed_model: &'a mut EdModel,
    size: &PhysicalSize<u32>,
    txt_coords: Vector2<f32>,
    config: &Config,
) -> EdResult<RenderedWgpu> {
    let glyph_dim_rect = ed_model.glyph_dim_rect_opt.context(MissingGlyphDims {})?;

    let mut all_text_sections = Vec::new();

    let (code_section, mut rects) = build_code_graphics(
        ed_model.markup_node_pool.get(ed_model.markup_root_id),
        size,
        txt_coords,
        config,
        glyph_dim_rect,
        &ed_model.markup_node_pool,
    )?;

    all_text_sections.push(code_section);

    let caret_w_sel_vec = ed_model
        .caret_w_select_vec
        .iter()
        .map(|(caret_w_sel, _)| *caret_w_sel)
        .collect();

    let mut sel_rects =
        build_selection_graphics(caret_w_sel_vec, txt_coords, config, glyph_dim_rect)?;

    rects.append(&mut sel_rects);

    if ed_model.show_debug_view {
        all_text_sections.push(build_debug_graphics(size, txt_coords, config, ed_model)?);
    }

    Ok(RenderedWgpu {
        text_sections: all_text_sections,
        rects,
    })
}

pub fn build_selection_graphics(
    caret_w_select_vec: Vec<CaretWSelect>,
    txt_coords: Vector2<f32>,
    config: &Config,
    glyph_dim_rect: Rect,
) -> EdResult<Vec<Rect>> {
    let mut rects = Vec::new();
    let char_width = glyph_dim_rect.width;
    let char_height = glyph_dim_rect.height;

    for caret_w_sel in caret_w_select_vec {
        let caret_row = caret_w_sel.caret_pos.line as f32;
        let caret_col = caret_w_sel.caret_pos.column as f32;

        let top_left_x = txt_coords.x + caret_col * char_width;

        let top_left_y = txt_coords.y + caret_row * char_height + 0.1 * char_height;

        rects.push(make_caret_rect(
            top_left_x,
            top_left_y,
            &glyph_dim_rect,
            &config.ed_theme.ui_theme,
        ))
    }

    Ok(rects)
}
