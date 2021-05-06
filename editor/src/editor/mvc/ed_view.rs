use super::ed_model::EdModel;
use crate::editor::config::Config;
use crate::editor::ed_error::EdResult;
use crate::editor::mvc::ed_model::SelectedExpression;
use crate::editor::render_ast::build_code_graphics;
use crate::editor::render_debug::build_debug_graphics;
use crate::editor::resources::strings::START_TIP;
use crate::graphics::primitives::rect::Rect;
use crate::graphics::primitives::text::{owned_section_from_text, Text};
use crate::lang::pool::Pool;
use crate::ui::text::caret_w_select::make_caret_rect;
use crate::ui::text::caret_w_select::make_selection_rect;
use crate::ui::text::caret_w_select::CaretWSelect;
use crate::ui::text::selection::Selection;
use crate::ui::ui_error::MissingGlyphDims;
use cgmath::Vector2;
use snafu::OptionExt;
use winit::dpi::PhysicalSize;

#[derive(Debug)]
pub struct RenderedWgpu {
    pub text_sections: Vec<glyph_brush::OwnedSection>,
    pub rects: Vec<Rect>,
}

impl RenderedWgpu {
    pub fn new() -> Self {
        Self {
            text_sections: Vec::new(),
            rects: Vec::new(),
        }
    }

    pub fn add_text(&mut self, new_text_section: glyph_brush::OwnedSection) {
        self.text_sections.push(new_text_section);
    }

    pub fn add_rect(&mut self, new_rect: Rect) {
        self.rects.push(new_rect);
    }

    pub fn add_rects(&mut self, new_rects: Vec<Rect>) {
        self.rects.extend(new_rects);
    }

    pub fn extend(&mut self, rendered_wgpu: RenderedWgpu) {
        self.text_sections.extend(rendered_wgpu.text_sections);
        self.rects.extend(rendered_wgpu.rects);
    }
}

// create text and rectangles based on EdModel's markup_root
pub fn model_to_wgpu<'a>(
    ed_model: &'a mut EdModel,
    size: &PhysicalSize<u32>,
    txt_coords: Vector2<f32>,
    config: &Config,
) -> EdResult<RenderedWgpu> {
    let glyph_dim_rect = ed_model.glyph_dim_rect_opt.context(MissingGlyphDims {})?;

    let mut all_rendered = RenderedWgpu::new();

    let tip_txt_coords = (txt_coords.x, txt_coords.y - 4.0 * config.code_font_size);

    let start_tip_text = owned_section_from_text(&Text {
        position: tip_txt_coords.into(),
        area_bounds: (size.width as f32, size.height as f32).into(),
        color: config.ed_theme.subtle_text,
        text: START_TIP,
        size: config.code_font_size,
        ..Default::default()
    });

    all_rendered.add_text(start_tip_text);

    let rendered_code_graphics = build_code_graphics(
        ed_model.markup_node_pool.get(ed_model.markup_root_id),
        size,
        txt_coords,
        config,
        glyph_dim_rect,
        &ed_model.markup_node_pool,
    )?;

    all_rendered.extend(rendered_code_graphics);

    let caret_w_sel_vec = ed_model
        .caret_w_select_vec
        .iter()
        .map(|(caret_w_sel, _)| *caret_w_sel)
        .collect();

    let rendered_selection = build_selection_graphics(
        caret_w_sel_vec,
        &ed_model.selected_expr_opt,
        txt_coords,
        config,
        glyph_dim_rect,
        ed_model.module.env.pool,
    )?;

    all_rendered.extend(rendered_selection);

    if ed_model.show_debug_view {
        all_rendered.add_text(build_debug_graphics(size, txt_coords, config, ed_model)?);
    }

    Ok(all_rendered)
}

pub fn build_selection_graphics(
    caret_w_select_vec: Vec<CaretWSelect>,
    _selected_expr_opt: &Option<SelectedExpression>,
    txt_coords: Vector2<f32>,
    config: &Config,
    glyph_dim_rect: Rect,
    _pool: &Pool,
) -> EdResult<RenderedWgpu> {
    let mut all_rendered = RenderedWgpu::new();
    let char_width = glyph_dim_rect.width;
    let char_height = glyph_dim_rect.height;

    let y_offset = 0.1 * char_height;

    for caret_w_sel in caret_w_select_vec {
        let caret_row = caret_w_sel.caret_pos.line as f32;
        let caret_col = caret_w_sel.caret_pos.column as f32;

        let top_left_x = txt_coords.x + caret_col * char_width;
        let top_left_y = txt_coords.y + caret_row * char_height + y_offset;

        if let Some(selection) = caret_w_sel.selection_opt {
            let Selection { start_pos, end_pos } = selection;

            let sel_rect_x = txt_coords.x + ((start_pos.column as f32) * char_width);
            let sel_rect_y = txt_coords.y + char_height * (start_pos.line as f32) + y_offset;

            let width =
                ((end_pos.column as f32) * char_width) - ((start_pos.column as f32) * char_width);

            all_rendered.add_rect(make_selection_rect(
                sel_rect_x,
                sel_rect_y,
                width,
                &glyph_dim_rect,
                &config.ed_theme.ui_theme,
            ));

            // render tooltip showing type
            /*if let Some(selected_expr) = selected_expr_opt {
                let tooltip = ToolTip {
                    position_x: sel_rect_x,
                    position_y: sel_rect_y - glyph_dim_rect.height,
                    text: selected_expr.type_str.as_str(pool),
                };

                let (tip_rect, tip_text) =
                    tooltip.render_tooltip(&glyph_dim_rect, &config.ed_theme.ui_theme);

                all_rendered.add_rect(tip_rect);
                all_rendered.add_text(tip_text);
            }*/
        }

        all_rendered.add_rect(make_caret_rect(
            top_left_x,
            top_left_y,
            &glyph_dim_rect,
            &config.ed_theme.ui_theme,
        ));
    }

    Ok(all_rendered)
}
