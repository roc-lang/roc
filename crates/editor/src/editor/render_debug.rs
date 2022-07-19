use crate::editor::ed_error::EdResult;
use crate::editor::mvc::ed_model::EdModel;
use crate::graphics::colors;
use crate::graphics::colors::from_hsb;
use crate::graphics::primitives::text as gr_text;
use cgmath::Vector2;
use roc_ast::lang::core::def::def2::def2_to_string;
use roc_code_markup::markup::nodes::tree_as_string;
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

    let debug_txt_coords: Vector2<f32> = (txt_coords.x * 20.0, txt_coords.y).into();

    let carets_text =
        glyph_brush::OwnedText::new(format!("carets: {:?}\n\n", ed_model.get_carets()))
            .with_color(colors::to_slice(from_hsb(0, 0, 100)))
            .with_scale(config.debug_font_size);

    let grid_node_map_text = glyph_brush::OwnedText::new(format!("{}", ed_model.grid_node_map))
        .with_color(colors::to_slice(from_hsb(20, 41, 100)))
        .with_scale(config.debug_font_size);

    let code_lines_text = glyph_brush::OwnedText::new(format!("{}", ed_model.code_lines))
        .with_color(colors::to_slice(from_hsb(0, 49, 96)))
        .with_scale(config.debug_font_size);

    let mut mark_node_trees_string = "\nmark node trees:".to_owned();

    for mark_id in ed_model.markup_ids[1..].iter() {
        // 1.. -> skip header
        mark_node_trees_string.push_str(&tree_as_string(*mark_id, &ed_model.mark_node_pool));
    }

    let mark_node_tree_text = glyph_brush::OwnedText::new(mark_node_trees_string)
        .with_color(colors::to_slice(from_hsb(266, 31, 96)))
        .with_scale(config.debug_font_size);

    let mark_node_pool_text = glyph_brush::OwnedText::new(
        ed_model
            .mark_node_pool
            .debug_string(&ed_model.mark_id_ast_id_map),
    )
    .with_color(colors::to_slice(from_hsb(110, 45, 82)))
    .with_scale(config.debug_font_size);

    let mut ast_node_text_str = "AST:\n".to_owned();

    for def_id in ed_model.module.ast.def_ids.iter() {
        ast_node_text_str.push_str(&def2_to_string(*def_id, ed_model.module.env.pool))
    }

    let ast_node_text = glyph_brush::OwnedText::new(ast_node_text_str)
        .with_color(colors::to_slice(from_hsb(211, 80, 100)))
        .with_scale(config.debug_font_size);

    let section = gr_text::owned_section_from_glyph_texts(
        vec![
            carets_text,
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
