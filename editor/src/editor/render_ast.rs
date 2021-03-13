use super::markup::{Attribute, MarkupNode};
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

// TODO return Rectangles for Box, caret, selection here as well
fn markup_to_wgpu<'a>(
    markup_node: &'a MarkupNode,
    code_style: &CodeStyle,
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
    )?;

    Ok((wgpu_texts, rects))
}

fn draw_attributes(
    attributes: &[Attribute],
    txt_row_col: &(usize, usize),
    code_style: &CodeStyle,
) -> Vec<Rect> {
    let char_width = code_style.glyph_dim_rect.width;

    attributes
        .iter()
        .map(|attr| match attr {
            Attribute::Caret { offset_col } => {
                let top_left_x = code_style.txt_coords.x
                    + (txt_row_col.1 as f32) * char_width
                    + (*offset_col as f32) * char_width;

                let top_left_y = code_style.txt_coords.y
                    + (txt_row_col.0 as f32) * char_width
                    + (*offset_col as f32) * char_width;

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
// TODO use attributes to render caret
fn markup_to_wgpu_helper<'a>(
    markup_node: &'a MarkupNode,
    wgpu_texts: &mut Vec<wgpu_glyph::Text<'a>>,
    rects: &mut Vec<Rect>,
    code_style: &CodeStyle,
    txt_row_col: &mut (usize, usize),
) -> EdResult<()> {
    match markup_node {
        MarkupNode::Nested {
            ast_node_id: _,
            children,
        } => {
            for child in children.iter() {
                markup_to_wgpu_helper(child, wgpu_texts, rects, code_style, txt_row_col)?;
            }
        }
        MarkupNode::Text {
            content,
            ast_node_id: _,
            syn_high_style,
            attributes,
        } => {
            let highlight_color = map_get(&code_style.ed_theme.syntax_high_map, &syn_high_style)?;

            let glyph_text = wgpu_glyph::Text::new(&content)
                .with_color(colors::to_slice(*highlight_color))
                .with_scale(code_style.font_size);

            rects.extend(draw_attributes(attributes, txt_row_col, code_style));
            txt_row_col.1 += content.len();
            wgpu_texts.push(glyph_text);
        }
        MarkupNode::Hole {
            ast_node_id: _,
            attributes,
            syn_high_style,
        } => {
            let hole_placeholder = " ";
            let glyph_text = wgpu_glyph::Text::new(hole_placeholder)
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

            txt_row_col.1 += hole_placeholder.len();
            wgpu_texts.push(glyph_text);
        }
    };

    Ok(())
}
