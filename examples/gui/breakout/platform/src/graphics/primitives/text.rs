// Adapted from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen - license information can be found in the COPYRIGHT
// file in the root directory of this distribution.
//
// Thank you, Benjamin!

use crate::graphics::colors::Rgba;
use crate::graphics::style::DEFAULT_FONT_SIZE;
use ab_glyph::{FontArc, InvalidFont};
use cgmath::Vector2;
use wgpu_glyph::{ab_glyph, GlyphBrush, GlyphBrushBuilder};

#[derive(Debug)]
pub struct Text<'a> {
    pub position: Vector2<f32>,
    pub area_bounds: Vector2<f32>,
    pub color: Rgba,
    pub text: &'a str,
    pub size: f32,
    pub visible: bool,
    pub centered: bool,
}

impl<'a> Default for Text<'a> {
    fn default() -> Self {
        Self {
            position: (0.0, 0.0).into(),
            area_bounds: (std::f32::INFINITY, std::f32::INFINITY).into(),
            color: Rgba::WHITE,
            text: "",
            size: DEFAULT_FONT_SIZE,
            visible: true,
            centered: false,
        }
    }
}

// pub fn layout_from_text(text: &Text) -> wgpu_glyph::Layout<wgpu_glyph::BuiltInLineBreaker> {
//     wgpu_glyph::Layout::default().h_align(if text.centered {
//         wgpu_glyph::HorizontalAlign::Center
//     } else {
//         wgpu_glyph::HorizontalAlign::Left
//     })
// }

// fn section_from_text<'a>(
//     text: &'a Text,
//     layout: wgpu_glyph::Layout<wgpu_glyph::BuiltInLineBreaker>,
// ) -> wgpu_glyph::Section<'a> {
//     Section {
//         screen_position: text.position.into(),
//         bounds: text.area_bounds.into(),
//         layout,
//         ..Section::default()
//     }
//     .add_text(
//         wgpu_glyph::Text::new(text.text)
//             .with_color(text.color)
//             .with_scale(text.size),
//     )
// }

// pub fn owned_section_from_text(text: &Text) -> OwnedSection {
//     let layout = layout_from_text(text);

//     OwnedSection {
//         screen_position: text.position.into(),
//         bounds: text.area_bounds.into(),
//         layout,
//         ..OwnedSection::default()
//     }
//     .add_text(
//         glyph_brush::OwnedText::new(text.text)
//             .with_color(Vector4::from(text.color))
//             .with_scale(text.size),
//     )
// }

// pub fn owned_section_from_glyph_texts(
//     text: Vec<glyph_brush::OwnedText>,
//     screen_position: (f32, f32),
//     area_bounds: (f32, f32),
//     layout: wgpu_glyph::Layout<wgpu_glyph::BuiltInLineBreaker>,
// ) -> glyph_brush::OwnedSection {
//     glyph_brush::OwnedSection {
//         screen_position,
//         bounds: area_bounds,
//         layout,
//         text,
//     }
// }

// pub fn queue_text_draw(text: &Text, glyph_brush: &mut GlyphBrush<()>) {
//     let layout = layout_from_text(text);

//     let section = section_from_text(text, layout);

//     glyph_brush.queue(section.clone());
// }

// fn glyph_to_rect(glyph: &wgpu_glyph::SectionGlyph) -> Rect {
//     let position = glyph.glyph.position;
//     let px_scale = glyph.glyph.scale;
//     let width = glyph_width(&glyph.glyph);
//     let height = px_scale.y;
//     let top_y = glyph_top_y(&glyph.glyph);

//     Rect {
//         pos: [position.x, top_y].into(),
//         width,
//         height,
//     }
// }

// pub fn glyph_top_y(glyph: &Glyph) -> f32 {
//     let height = glyph.scale.y;

//     glyph.position.y - height * 0.75
// }

// pub fn glyph_width(glyph: &Glyph) -> f32 {
//     glyph.scale.x * 0.4765
// }

pub fn build_glyph_brush(
    gpu_device: &wgpu::Device,
    render_format: wgpu::TextureFormat,
) -> Result<GlyphBrush<()>, InvalidFont> {
    let inconsolata = FontArc::try_from_slice(include_bytes!(
        "../../../../../../crates/editor/Inconsolata-Regular.ttf"
    ))?;

    Ok(GlyphBrushBuilder::using_font(inconsolata).build(gpu_device, render_format))
}
