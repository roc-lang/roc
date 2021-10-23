use super::ed_model::EdModel;
use crate::editor::config::Config;
use crate::editor::ed_error::EdResult;
use crate::editor::mvc::ed_model::SelectedBlock;
use crate::editor::render_ast::build_code_graphics;
use crate::editor::render_debug::build_debug_graphics;
use crate::editor::resources::strings::START_TIP;
use crate::graphics::primitives::rect::Rect;
use crate::graphics::primitives::text::{owned_section_from_text, Text};
use crate::ui::text::caret_w_select::make_caret_rect;
use crate::ui::text::caret_w_select::make_selection_rect;
use crate::ui::text::caret_w_select::CaretWSelect;
use crate::ui::text::selection::Selection;
use crate::ui::tooltip::ToolTip;
use crate::ui::ui_error::MissingGlyphDims;
use cgmath::Vector2;
use roc_ast::mem_pool::pool::Pool;
use snafu::OptionExt;
use winit::dpi::PhysicalSize;

#[derive(Debug)]
pub struct RenderedWgpu {
    pub text_sections_behind: Vec<glyph_brush::OwnedSection>, // displayed in front of rect_behind, behind everything else
    pub text_sections_front: Vec<glyph_brush::OwnedSection>,  // displayed in front of everything
    pub rects_behind: Vec<Rect>,                              // displayed at lowest depth
    pub rects_front: Vec<Rect>, // displayed in front of text_sections_behind, behind text_sections_front
}

impl RenderedWgpu {
    pub fn new() -> Self {
        Self {
            text_sections_behind: Vec::new(),
            text_sections_front: Vec::new(),
            rects_behind: Vec::new(),
            rects_front: Vec::new(),
        }
    }

    pub fn add_text_behind(&mut self, new_text_section: glyph_brush::OwnedSection) {
        self.text_sections_behind.push(new_text_section);
    }

    pub fn add_text_front(&mut self, new_text_section: glyph_brush::OwnedSection) {
        self.text_sections_front.push(new_text_section);
    }

    pub fn add_rect_behind(&mut self, new_rect: Rect) {
        self.rects_behind.push(new_rect);
    }

    pub fn add_rects_behind(&mut self, new_rects: Vec<Rect>) {
        self.rects_behind.extend(new_rects);
    }

    pub fn add_rect_front(&mut self, new_rect: Rect) {
        self.rects_front.push(new_rect);
    }

    pub fn extend(&mut self, rendered_wgpu: RenderedWgpu) {
        self.text_sections_behind
            .extend(rendered_wgpu.text_sections_behind);
        self.text_sections_front
            .extend(rendered_wgpu.text_sections_front);
        self.rects_behind.extend(rendered_wgpu.rects_behind);
        self.rects_front.extend(rendered_wgpu.rects_front);
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

    let tip_txt_coords = (
        txt_coords.x,
        txt_coords.y - (START_TIP.matches('\n').count() as f32 + 1.0) * config.code_font_size,
    );

    let start_tip_text = owned_section_from_text(&Text {
        position: tip_txt_coords.into(),
        area_bounds: (size.width as f32, size.height as f32).into(),
        color: config.ed_theme.subtle_text,
        text: START_TIP,
        size: config.code_font_size,
        ..Default::default()
    });

    all_rendered.add_text_behind(start_tip_text);

    let rendered_code_graphics = build_code_graphics(
        &ed_model.markup_ids,
        size,
        txt_coords,
        config,
        glyph_dim_rect,
        &ed_model.mark_node_pool,
    )?;

    all_rendered.extend(rendered_code_graphics);

    let caret_w_sel_vec = ed_model
        .caret_w_select_vec
        .iter()
        .map(|(caret_w_sel, _)| *caret_w_sel)
        .collect();

    let rendered_selection = build_selection_graphics(
        caret_w_sel_vec,
        &ed_model.selected_block_opt,
        txt_coords,
        config,
        glyph_dim_rect,
        ed_model.module.env.pool,
    )?;

    all_rendered.extend(rendered_selection);

    if ed_model.show_debug_view {
        all_rendered.add_text_behind(build_debug_graphics(size, txt_coords, config, ed_model)?);
    }

    Ok(all_rendered)
}

pub fn build_selection_graphics(
    caret_w_select_vec: Vec<CaretWSelect>,
    selected_expr_opt: &Option<SelectedBlock>,
    txt_coords: Vector2<f32>,
    config: &Config,
    glyph_dim_rect: Rect,
    pool: &Pool,
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

            all_rendered.add_rect_behind(make_selection_rect(
                sel_rect_x,
                sel_rect_y,
                width,
                &glyph_dim_rect,
                &config.ed_theme.ui_theme,
            ));

            // render tooltip showing type
            if let Some(selected_expr) = selected_expr_opt {
                let tooltip = ToolTip {
                    position_x: sel_rect_x,
                    position_y: sel_rect_y - glyph_dim_rect.height,
                    text: selected_expr.type_str.as_str(pool),
                };

                let (tip_rect, tip_text) = tooltip.render_tooltip(
                    &glyph_dim_rect,
                    &config.ed_theme.ui_theme,
                    config.code_font_size,
                );

                all_rendered.add_rect_front(tip_rect);
                all_rendered.add_text_front(tip_text);
            }
        }

        all_rendered.add_rect_front(make_caret_rect(
            top_left_x,
            top_left_y,
            &glyph_dim_rect,
            &config.ed_theme.ui_theme,
        ));
    }

    Ok(all_rendered)
}
