use crate::editor::{
    ed_error::EdResult,
    syntax_highlight::{HighlightStyle},
    util::map_get,
};
use super::markup::{MarkupNode, expr2_to_markup};
use crate::graphics::colors::RgbaTup;
use crate::graphics::primitives::text as gr_text;
use crate::graphics::primitives::rect::Rect;
use bumpalo::Bump;
use cgmath::Vector2;
use std::collections::HashMap;
use winit::dpi::PhysicalSize;

use crate::{
    editor::config::Config,
    graphics::colors,
    lang::{ast::Expr2, expr::Env},
};

pub fn expr2_to_wgpu<'a>(
    markup_root: &'a mut MarkupNode,
    arena: &'a Bump,
    env: &mut Env<'a>,
    expr2: &Expr2,
    size: &PhysicalSize<u32>,
    position: Vector2<f32>,
    config: &Config,
    glyph_dim_rect: Rect,
) -> EdResult<(wgpu_glyph::Section<'a>, Vec<Rect>)> {
    *markup_root = expr2_to_markup(arena, env, expr2);

    build_code_graphics(markup_root, size, position, config, glyph_dim_rect)
}

pub fn build_code_graphics<'a>(
    markup_node: &'a MarkupNode,
    size: &PhysicalSize<u32>,
    position: Vector2<f32>,
    config: &Config,
    glyph_dim_rect: Rect,
) -> EdResult<(wgpu_glyph::Section<'a>, Vec<Rect>)> {
    let area_bounds = (size.width as f32, size.height as f32);
    let layout = wgpu_glyph::Layout::default().h_align(wgpu_glyph::HorizontalAlign::Left);

    let (glyph_text_vec, rects) = markup_to_wgpu(
        markup_node,
        &config.ed_theme.syntax_high_map,
        config.code_font_size,
        glyph_dim_rect,
    )?;

    let section =
        gr_text::section_from_glyph_text(glyph_text_vec, position.into(), area_bounds, layout);

    Ok((section, rects))
}

// TODO return Rectangles for Box, caret, selection here as well
fn markup_to_wgpu<'a>(
    markup_node: &'a MarkupNode,
    syntax_theme: &HashMap<HighlightStyle, RgbaTup>,
    font_size: f32,
    glyph_dim_rect: Rect,
) -> EdResult<(Vec<wgpu_glyph::Text<'a>>, Vec<Rect>)> {
    let mut wgpu_texts: Vec<wgpu_glyph::Text<'a>> = Vec::new();
    let mut rects: Vec<Rect> = Vec::new();
    
    let mut text_row = 0;
    let mut text_col = 0;

    markup_to_wgpu_helper(markup_node, &mut wgpu_texts, &mut rects, syntax_theme, font_size, &mut text_row, &mut text_col, glyph_dim_rect)?;

    Ok((wgpu_texts, rects))
}

// TODO use text_row
fn markup_to_wgpu_helper<'a>(
    markup_node: &'a MarkupNode,
    wgpu_texts: &mut Vec<wgpu_glyph::Text<'a>>,
    rects: &mut Vec<Rect>,
    syntax_theme: &HashMap<HighlightStyle, RgbaTup>,
    font_size: f32,
    text_row: &mut usize,
    text_col: &mut usize,
    glyph_dim_rect: Rect,
) -> EdResult<()> {
    match markup_node {
        MarkupNode::Nested {ast_node_id: _, children} => 
            for child in children.iter() {
                markup_to_wgpu_helper(child, wgpu_texts, rects, syntax_theme, font_size, text_row, text_col, glyph_dim_rect)?;
            },
        MarkupNode::Text {content, ast_node_id: _, syn_high_style, attributes:_} => {
                let highlight_color = map_get(&syntax_theme, &syn_high_style)?;

                let glyph_text = wgpu_glyph::Text::new(&content)
                    .with_color(colors::to_slice(*highlight_color))
                    .with_scale(font_size);
                
                *text_col += content.len();
                wgpu_texts.push(glyph_text);
            },
        MarkupNode::Hole { ast_node_id: _, attributes: _} => 
            {
                let hole_placeholder = " ";
                let glyph_text = wgpu_glyph::Text::new(hole_placeholder)
                        .with_color(colors::to_slice(colors::WHITE))
                        .with_scale(font_size);

                let hole_rect = Rect {
                    top_left_coords: ((*text_row as f32) * glyph_dim_rect.height, (*text_col as f32) * glyph_dim_rect.width).into(),
                    width: glyph_dim_rect.width,
                    height: glyph_dim_rect.height,
                    color: colors::WHITE,
                };
                rects.push(hole_rect);
                
                *text_col += hole_placeholder.len();
                wgpu_texts.push(glyph_text);
            }
    };

    Ok(())
}
