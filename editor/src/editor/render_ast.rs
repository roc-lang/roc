use super::markup::nodes::{MarkupNode, BLANK_PLACEHOLDER};
use super::slow_pool::MarkNodeId;
use crate::editor::mvc::ed_view::RenderedWgpu;
use crate::editor::slow_pool::SlowPool;
use crate::editor::{ed_error::EdResult, theme::EdTheme, util::map_get};
use crate::graphics::primitives::rect::Rect;
use crate::graphics::primitives::text as gr_text;
use cgmath::Vector2;
use winit::dpi::PhysicalSize;

use crate::{editor::config::Config, graphics::colors};

pub fn build_code_graphics<'a>(
    markup_ids: &[MarkNodeId],
    size: &PhysicalSize<u32>,
    txt_coords: Vector2<f32>,
    config: &Config,
    glyph_dim_rect: Rect,
    markup_node_pool: &'a SlowPool,
) -> EdResult<RenderedWgpu> {
    let area_bounds = (size.width as f32, size.height as f32);
    let layout = wgpu_glyph::Layout::default().h_align(wgpu_glyph::HorizontalAlign::Left);
    let mut rendered_wgpu = RenderedWgpu::new();

    let mut all_glyph_text_vec = vec![];
    let mut all_rects = vec![];
    let mut txt_row_col = (0, 0);

    for markup_id in markup_ids.iter() {
        let mark_node = markup_node_pool.get(*markup_id);

        let (mut glyph_text_vec, mut rects) = markup_to_wgpu(
            mark_node,
            &CodeStyle {
                ed_theme: &config.ed_theme,
                font_size: config.code_font_size,
                txt_coords,
                glyph_dim_rect,
            },
            &mut txt_row_col,
            markup_node_pool,
        )?;

        // on to next line
        txt_row_col.0 += 1;
        txt_row_col.1 = 0;

        all_glyph_text_vec.append(&mut glyph_text_vec);
        all_rects.append(&mut rects)
    }

    let section = gr_text::owned_section_from_glyph_texts(
        all_glyph_text_vec,
        txt_coords.into(),
        area_bounds,
        layout,
    );

    rendered_wgpu.add_rects(all_rects);
    rendered_wgpu.add_text(section);

    Ok(rendered_wgpu)
}

struct CodeStyle<'a> {
    ed_theme: &'a EdTheme,
    font_size: f32,
    txt_coords: Vector2<f32>,
    glyph_dim_rect: Rect,
}

fn markup_to_wgpu<'a>(
    markup_node: &'a MarkupNode,
    code_style: &CodeStyle,
    txt_row_col: &mut (usize, usize),
    markup_node_pool: &'a SlowPool,
) -> EdResult<(Vec<glyph_brush::OwnedText>, Vec<Rect>)> {
    let mut wgpu_texts: Vec<glyph_brush::OwnedText> = Vec::new();
    let mut rects: Vec<Rect> = Vec::new();

    markup_to_wgpu_helper(
        markup_node,
        &mut wgpu_texts,
        &mut rects,
        code_style,
        txt_row_col,
        markup_node_pool,
    )?;

    Ok((wgpu_texts, rects))
}

// TODO use text_row
fn markup_to_wgpu_helper<'a>(
    markup_node: &'a MarkupNode,
    wgpu_texts: &mut Vec<glyph_brush::OwnedText>,
    rects: &mut Vec<Rect>,
    code_style: &CodeStyle,
    txt_row_col: &mut (usize, usize),
    markup_node_pool: &'a SlowPool,
) -> EdResult<()> {
    match markup_node {
        MarkupNode::Nested {
            ast_node_id: _,
            children_ids,
            parent_id_opt: _,
        } => {
            for child_id in children_ids.iter() {
                let child = markup_node_pool.get(*child_id);
                markup_to_wgpu_helper(
                    child,
                    wgpu_texts,
                    rects,
                    code_style,
                    txt_row_col,
                    markup_node_pool,
                )?;
            }
        }
        MarkupNode::Text {
            content,
            ast_node_id: _,
            syn_high_style,
            attributes: _,
            parent_id_opt: _,
        } => {
            let highlight_color = map_get(&code_style.ed_theme.syntax_high_map, &syn_high_style)?;

            let glyph_text = glyph_brush::OwnedText::new(content)
                .with_color(colors::to_slice(*highlight_color))
                .with_scale(code_style.font_size);

            if content.contains("\n") {
                txt_row_col.0 += 1;
                txt_row_col.1 = 0;
            }

            txt_row_col.1 += content.len();
            wgpu_texts.push(glyph_text);
        }
        MarkupNode::Blank {
            ast_node_id: _,
            attributes: _,
            syn_high_style,
            parent_id_opt: _,
        } => {
            let glyph_text = glyph_brush::OwnedText::new(BLANK_PLACEHOLDER)
                .with_color(colors::to_slice(colors::WHITE))
                .with_scale(code_style.font_size);

            let highlight_color = map_get(&code_style.ed_theme.syntax_high_map, &syn_high_style)?;

            let char_width = code_style.glyph_dim_rect.width;
            let char_height = code_style.glyph_dim_rect.height;

            let blank_rect = Rect {
                top_left_coords: (
                    code_style.txt_coords.x + (txt_row_col.1 as f32) * char_width,
                    code_style.txt_coords.y
                        + ((txt_row_col.0 - 1) as f32) * char_height
                        + 0.1 * char_height,
                )
                    .into(),
                width: char_width,
                height: char_height,
                color: *highlight_color,
            };
            rects.push(blank_rect);

            txt_row_col.1 += BLANK_PLACEHOLDER.len();
            wgpu_texts.push(glyph_text);
        }
    };

    Ok(())
}
