// Adapted from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen, licensed under the MIT license

use super::rect::Rect;
use crate::graphics::colors::CODE_COLOR;
use crate::graphics::style::CODE_FONT_SIZE;
use ab_glyph::{FontArc, Glyph, InvalidFont};
use cgmath::{Vector2, Vector4};
use itertools::Itertools;
use wgpu_glyph::{ab_glyph, GlyphBrush, GlyphBrushBuilder, GlyphCruncher, Section};

#[derive(Debug)]
pub struct Text {
    pub position: Vector2<f32>,
    pub area_bounds: Vector2<f32>,
    pub color: Vector4<f32>,
    pub text: String,
    pub size: f32,
    pub visible: bool,
    pub centered: bool,
}

impl Default for Text {
    fn default() -> Self {
        Self {
            position: (0.0, 0.0).into(),
            area_bounds: (std::f32::INFINITY, std::f32::INFINITY).into(),
            color: (1.0, 1.0, 1.0, 1.0).into(),
            text: String::new(),
            size: CODE_FONT_SIZE,
            visible: true,
            centered: false,
        }
    }
}

// necessary to get dimensions for caret
pub fn example_code_glyph_rect(glyph_brush: &mut GlyphBrush<()>) -> Rect {
    let code_text = Text {
        position: (30.0, 90.0).into(), //TODO 30.0 90.0 should be an arg
        area_bounds: (std::f32::INFINITY, std::f32::INFINITY).into(),
        color: CODE_COLOR.into(),
        text: "a".to_owned(),
        size: CODE_FONT_SIZE,
        ..Default::default()
    };

    let layout = layout_from_text(&code_text);

    let section = section_from_text(&code_text, layout);

    let mut glyph_section_iter = glyph_brush.glyphs_custom_layout(section, &layout);

    if let Some(glyph) = glyph_section_iter.next() {
        glyph_to_rect(glyph)
    } else {
        unreachable!();
    }
}

fn layout_from_text(text: &Text) -> wgpu_glyph::Layout<wgpu_glyph::BuiltInLineBreaker> {
    wgpu_glyph::Layout::default().h_align(if text.centered {
        wgpu_glyph::HorizontalAlign::Center
    } else {
        wgpu_glyph::HorizontalAlign::Left
    })
}

fn section_from_text(
    text: &Text,
    layout: wgpu_glyph::Layout<wgpu_glyph::BuiltInLineBreaker>,
) -> wgpu_glyph::Section {
    Section {
        screen_position: text.position.into(),
        bounds: text.area_bounds.into(),
        layout,
        ..Section::default()
    }
    .add_text(
        wgpu_glyph::Text::new(&text.text)
            .with_color(text.color)
            .with_scale(text.size),
    )
}

// returns bounding boxes for every glyph
pub fn queue_text_draw(text: &Text, glyph_brush: &mut GlyphBrush<()>) -> Vec<Vec<Rect>> {
    let layout = layout_from_text(text);

    let section = section_from_text(text, layout);

    glyph_brush.queue(section.clone());

    let glyph_section_iter = glyph_brush.glyphs_custom_layout(section, &layout);

    glyph_section_iter
        .map(glyph_to_rect)
        .group_by(|rect| rect.top_left_coords.y)
        .into_iter()
        .map(|(_y_coord, rect_group)| {
            let mut rects_vec = rect_group.collect::<Vec<Rect>>();
            let last_rect_opt = rects_vec.last().cloned();
            // add extra rect to make it easy to highlight the newline character
            if let Some(last_rect) = last_rect_opt {
                rects_vec.push(Rect {
                    top_left_coords: [
                        last_rect.top_left_coords.x + last_rect.width,
                        last_rect.top_left_coords.y,
                    ]
                    .into(),
                    width: last_rect.width,
                    height: last_rect.height,
                    color: last_rect.color,
                });
            }
            rects_vec
        })
        .collect()
}

fn glyph_to_rect(glyph: &wgpu_glyph::SectionGlyph) -> Rect {
    let position = glyph.glyph.position;
    let px_scale = glyph.glyph.scale;
    let width = glyph_width(&glyph.glyph);
    let height = px_scale.y;
    let top_y = glyph_top_y(&glyph.glyph);

    Rect {
        top_left_coords: [position.x, top_y].into(),
        width,
        height,
        color: [1.0, 1.0, 1.0],
    }
}

pub fn glyph_top_y(glyph: &Glyph) -> f32 {
    let height = glyph.scale.y;

    glyph.position.y - height * 0.75
}

pub fn glyph_width(glyph: &Glyph) -> f32 {
    glyph.scale.x * 0.5
}

pub fn build_glyph_brush(
    gpu_device: &wgpu::Device,
    render_format: wgpu::TextureFormat,
) -> Result<GlyphBrush<()>, InvalidFont> {
    let inconsolata = FontArc::try_from_slice(include_bytes!("../../../Inconsolata-Regular.ttf"))?;

    Ok(GlyphBrushBuilder::using_font(inconsolata).build(&gpu_device, render_format))
}
