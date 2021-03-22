use super::ed_model::EdModel;
use crate::editor::config::Config;
use crate::editor::ed_error::EdResult;
use crate::editor::render_ast::build_code_graphics;
use crate::editor::slow_pool::SlowPool;
use crate::graphics::primitives::rect::Rect;
use crate::ui::ui_error::MissingGlyphDims;
use cgmath::Vector2;
use snafu::OptionExt;
use winit::dpi::PhysicalSize;

// create text and rectangles based on EdModel's markup_root
pub fn model_to_wgpu<'a>(
    ed_model: &'a mut EdModel,
    size: &PhysicalSize<u32>,
    txt_coords: Vector2<f32>,
    config: &Config,
    markup_node_pool: &'a SlowPool,
) -> EdResult<(wgpu_glyph::Section<'a>, Vec<Rect>)> {
    let glyph_dim_rect = ed_model.glyph_dim_rect_opt.context(MissingGlyphDims {})?;

    build_code_graphics(
        markup_node_pool.get(ed_model.markup_root_id),
        size,
        txt_coords,
        config,
        glyph_dim_rect,
        markup_node_pool,
    )

    //TODO implement method
    build_selection_graphics(
        ed_model.caret_w_select_vec
        size,
        txt_coords,
        config,
        glyph_dim_rect,
    )
}
