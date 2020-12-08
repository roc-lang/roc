// Adapted from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen, licensed under the MIT license

use ab_glyph::{FontArc, InvalidFont};
use cgmath::{Vector2, Vector4};
use wgpu_glyph::{ab_glyph, GlyphBrush, GlyphBrushBuilder, Section};

#[derive(Debug)]
pub struct Text {
    pub position: Vector2<f32>,
    pub bounds: Vector2<f32>,
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
            bounds: (std::f32::INFINITY, std::f32::INFINITY).into(),
            color: (1.0, 1.0, 1.0, 1.0).into(),
            text: String::new(),
            size: 16.0,
            visible: true,
            centered: false,
        }
    }
}

pub fn queue_text_draw(text: &Text, glyph_brush: &mut GlyphBrush<()>) {
    let layout = wgpu_glyph::Layout::default().h_align(if text.centered {
        wgpu_glyph::HorizontalAlign::Center
    } else {
        wgpu_glyph::HorizontalAlign::Left
    });

    let section = Section {
        screen_position: text.position.into(),
        bounds: text.bounds.into(),
        layout,
        ..Section::default()
    }
    .add_text(
        wgpu_glyph::Text::new(&text.text)
            .with_color(text.color)
            .with_scale(text.size),
    );

    glyph_brush.queue(section);
}

pub fn build_glyph_brush(
    gpu_device: &wgpu::Device,
    render_format: wgpu::TextureFormat,
) -> Result<GlyphBrush<()>, InvalidFont> {
    let inconsolata = FontArc::try_from_slice(include_bytes!("../Inconsolata-Regular.ttf"))?;

    Ok(GlyphBrushBuilder::using_font(inconsolata).build(&gpu_device, render_format))
}
