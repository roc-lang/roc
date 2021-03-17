use super::markup::nodes::{MarkupNode, BLANK_PLACEHOLDER};
use super::markup::attribute::{Attribute, Attributes};
use crate::editor::slow_pool::SlowPool;
use crate::editor::{ed_error::EdResult, theme::EdTheme, util::map_get};
use crate::graphics::primitives::rect::Rect;
use crate::graphics::primitives::text as gr_text;
use crate::ui::text::caret_w_select::make_caret_rect;
use cgmath::Vector2;
use winit::dpi::PhysicalSize;

use crate::{editor::config::Config, graphics::colors};

pub fn build_code_graphics<'a>(
    markup_node: &'a MarkupNode,
    size: &PhysicalSize<u32>,
    txt_coords: Vector2<f32>,
    config: &Config,
    glyph_dim_rect: Rect,
    markup_node_pool: &'a SlowPool,
) -> EdResult<(wgpu_glyph::Section<'a>, Vec<Rect>)> {
    let area_bounds = (size.width as f32, size.height as f32);
    let layout = wgpu_glyph::Layout::default().h_align(wgpu_glyph::HorizontalAlign::Left);

    let (glyph_text_vec, rects) = markup_to_wgpu(
        markup_node,
        &CodeStyle {
            ed_theme: &config.ed_theme,
            font_size: config.code_font_size,
            txt_coords,
            glyph_dim_rect,
        },
        markup_node_pool,
    )?;

    let section =
        gr_text::section_from_glyph_text(glyph_text_vec, txt_coords.into(), area_bounds, layout);

    Ok((section, rects))
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
    markup_node_pool: &'a SlowPool,
) -> EdResult<(Vec<wgpu_glyph::Text<'a>>, Vec<Rect>)> {
    let mut wgpu_texts: Vec<wgpu_glyph::Text<'a>> = Vec::new();
    let mut rects: Vec<Rect> = Vec::new();

    let mut txt_row_col = (0, 0);

    markup_to_wgpu_helper(
        markup_node,
        &mut wgpu_texts,
        &mut rects,
        code_style,
        &mut txt_row_col,
        markup_node_pool,
    )?;

    Ok((wgpu_texts, rects))
}

fn draw_attributes(
    attributes: &Attributes,
    txt_row_col: &(usize, usize),
    code_style: &CodeStyle,
) -> Vec<Rect> {
    let char_width = code_style.glyph_dim_rect.width;

    attributes
        .all
        .iter()
        .map(|attr| match attr {
            Attribute::Caret { caret } => {
                let caret_col = caret.offset_col as f32;

                let top_left_x = code_style.txt_coords.x
                    + (txt_row_col.1 as f32) * char_width
                    + caret_col * char_width;

                let top_left_y = code_style.txt_coords.y
                    + (txt_row_col.0 as f32) * char_width
                    + char_width * 0.2;

                make_caret_rect(
                    top_left_x,
                    top_left_y,
                    &code_style.glyph_dim_rect,
                    &code_style.ed_theme.ui_theme,
                )
            }
            rest => todo!("implement draw_attributes for {:?}", rest),
        })
        .collect()
}

// TODO use text_row
fn markup_to_wgpu_helper<'a>(
    markup_node: &'a MarkupNode,
    wgpu_texts: &mut Vec<wgpu_glyph::Text<'a>>,
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
            attributes,
            parent_id_opt: _,
        } => {
            let highlight_color = map_get(&code_style.ed_theme.syntax_high_map, &syn_high_style)?;

            let glyph_text = wgpu_glyph::Text::new(&content)
                .with_color(colors::to_slice(*highlight_color))
                .with_scale(code_style.font_size);

            rects.extend(draw_attributes(attributes, txt_row_col, code_style));
            txt_row_col.1 += content.len();
            wgpu_texts.push(glyph_text);
        }
        MarkupNode::Blank {
            ast_node_id: _,
            attributes,
            syn_high_style,
            parent_id_opt: _,
        } => {
            let glyph_text = wgpu_glyph::Text::new(BLANK_PLACEHOLDER)
                .with_color(colors::to_slice(colors::WHITE))
                .with_scale(code_style.font_size);

            let highlight_color = map_get(&code_style.ed_theme.syntax_high_map, &syn_high_style)?;

            let hole_rect = Rect {
                top_left_coords: (
                    code_style.txt_coords.x
                        + (txt_row_col.0 as f32) * code_style.glyph_dim_rect.height,
                    code_style.txt_coords.y
                        + (txt_row_col.1 as f32) * code_style.glyph_dim_rect.width,
                )
                    .into(),
                width: code_style.glyph_dim_rect.width,
                height: code_style.glyph_dim_rect.height,
                color: *highlight_color,
            };
            rects.push(hole_rect);

            rects.extend(draw_attributes(attributes, txt_row_col, code_style));

            txt_row_col.1 += BLANK_PLACEHOLDER.len();
            wgpu_texts.push(glyph_text);
        }
    };

    Ok(())
}
