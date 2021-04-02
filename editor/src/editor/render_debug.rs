use crate::graphics::colors;
use crate::graphics::colors::from_hsb;
use crate::editor::mvc::ed_model::EdModel;
use crate::editor::{ed_error::EdResult};
use crate::graphics::primitives::text as gr_text;
use cgmath::Vector2;
use winit::dpi::PhysicalSize;

use crate::{editor::config::Config};

pub fn build_debug_graphics(
    size: &PhysicalSize<u32>,
    txt_coords: Vector2<f32>,
    config: &Config,
    ed_model: &EdModel,
) -> EdResult<glyph_brush::OwnedSection> {
    let area_bounds = (size.width as f32, size.height as f32);
    let layout = wgpu_glyph::Layout::default().h_align(wgpu_glyph::HorizontalAlign::Left);

    let debug_txt_coords: Vector2<f32> = (txt_coords.x, txt_coords.y * 6.0).into();

    let grid_node_map_text = 
        glyph_brush::OwnedText::new(
            format!("{}", ed_model.grid_node_map)
        )
        .with_color(colors::to_slice(from_hsb(25, 82, 96)))
        .with_scale(config.code_font_size);

    let code_lines_text =
        glyph_brush::OwnedText::new(
            format!("{}", ed_model.code_lines)
        )
        .with_color(colors::to_slice(from_hsb(73, 22, 78)))
        .with_scale(config.code_font_size);

    let mark_node_pool_text =
        glyph_brush::OwnedText::new(
            format!("{}", ed_model.markup_node_pool)
        )
        .with_color(colors::to_slice(from_hsb(151, 36, 44)))
        .with_scale(config.code_font_size);    

    let section =
        gr_text::section_from_glyph_text(
            vec![
                grid_node_map_text,
                code_lines_text,
                mark_node_pool_text
            ],
            debug_txt_coords.into(),
            area_bounds,
            layout
        );

    Ok(section)
}