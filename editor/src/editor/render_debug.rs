use crate::editor::ed_error::EdResult;
use crate::editor::markup::nodes::tree_as_string;
use crate::editor::mvc::ed_model::EdModel;
use crate::graphics::colors;
use crate::graphics::colors::from_hsb;
use crate::graphics::primitives::text as gr_text;
use crate::lang::ast::expr2_to_string;
use cgmath::Vector2;
use winit::dpi::PhysicalSize;

use crate::editor::config::Config;

pub fn build_debug_graphics(
    size: &PhysicalSize<u32>,
    txt_coords: Vector2<f32>,
    config: &Config,
    ed_model: &EdModel,
) -> EdResult<glyph_brush::OwnedSection> {
    let area_bounds = (size.width as f32, size.height as f32);
    let layout = wgpu_glyph::Layout::default().h_align(wgpu_glyph::HorizontalAlign::Left);

    let debug_txt_coords: Vector2<f32> = (txt_coords.x, txt_coords.y * 6.0).into();

    let grid_node_map_text = glyph_brush::OwnedText::new(format!("{}", ed_model.grid_node_map))
        .with_color(colors::to_slice(from_hsb(20, 41, 100)))
        .with_scale(config.code_font_size);

    let code_lines_text = glyph_brush::OwnedText::new(format!("{}", ed_model.code_lines))
        .with_color(colors::to_slice(from_hsb(0, 49, 96)))
        .with_scale(config.code_font_size);

    let mark_node_tree_text = glyph_brush::OwnedText::new(tree_as_string(
        ed_model.markup_root_id,
        &ed_model.markup_node_pool,
    ))
    .with_color(colors::to_slice(from_hsb(266, 31, 96)))
    .with_scale(config.code_font_size);

    let mark_node_pool_text = glyph_brush::OwnedText::new(format!("{}", ed_model.markup_node_pool))
        .with_color(colors::to_slice(from_hsb(110, 45, 82)))
        .with_scale(config.code_font_size);

    let ast_node_text = glyph_brush::OwnedText::new(format!(
        "\n\n(ast_root)\n{}",
        expr2_to_string(ed_model.module.ast_root_id, ed_model.module.env.pool)
    ))
    .with_color(colors::to_slice(from_hsb(211, 80, 100)))
    .with_scale(config.code_font_size);

    let section = gr_text::owned_section_from_glyph_texts(
        vec![
            grid_node_map_text,
            code_lines_text,
            mark_node_tree_text,
            mark_node_pool_text,
            ast_node_text,
        ],
        debug_txt_coords.into(),
        area_bounds,
        layout,
    );

    Ok(section)
}
