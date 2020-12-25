// Adapted from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen, licensed under the MIT license

use ab_glyph::{FontArc, InvalidFont, Glyph};
use cgmath::{Vector2, Vector4};
use wgpu_glyph::{ab_glyph, GlyphBrush, GlyphBrushBuilder, GlyphCruncher, Section};
use crate::rect::Rect;
use itertools::Itertools;

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
            size: 16.0,
            visible: true,
            centered: false,
        }
    }
}

// returns bounding boxes for every glyph
pub fn queue_text_draw(text: &Text, glyph_brush: &mut GlyphBrush<()>) -> Vec<Vec<Rect>> {
    let layout = wgpu_glyph::Layout::default().h_align(if text.centered {
        wgpu_glyph::HorizontalAlign::Center
    } else {
        wgpu_glyph::HorizontalAlign::Left
    });

    let section = Section {
        screen_position: text.position.into(),
        bounds: text.area_bounds.into(),
        layout,
        ..Section::default()
    }.add_text(
        wgpu_glyph::Text::new(&text.text)
            .with_color(text.color)
            .with_scale(text.size),
    );

    glyph_brush.queue(section.clone());

    let glyph_section_iter = glyph_brush.glyphs_custom_layout(section, &layout);

    glyph_section_iter.map(|section_glyph|
        {
            let position = section_glyph.glyph.position;
            let px_scale = section_glyph.glyph.scale;
            let width = glyph_width(&section_glyph.glyph);
            let height = px_scale.y;
            let top_y = glyph_top_y(&section_glyph.glyph);

            Rect {
                top_left_coords: [position.x, top_y].into(),
                width,
                height,
                color: [1.0, 1.0, 1.0]
            }
        }
    ).group_by(|rect| rect.top_left_coords.y)
    .into_iter()
    .map(|(_y_coord, rect_group)| rect_group.collect())
    .collect()
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
    let inconsolata = FontArc::try_from_slice(include_bytes!("../Inconsolata-Regular.ttf"))?;

    Ok(GlyphBrushBuilder::using_font(inconsolata).build(&gpu_device, render_format))
}

/*fn my_draw_text(&mut self, ...) {
    let layout = ...;
    let section = ...;

    // Avoid re-allocating new vec's every time by storing them internally
    self.last_glyphs.clear();
    self.last_bounds.clear();

    // Get all the glyphs for the current section to calculate their bounds. Due to
    // mutable borrow, this must be stored first.
    self.last_glyphs
        .extend(self.brush.glyphs_custom_layout(&s, &layout).cloned());

    // Calculate the bounds of each glyph
    self.last_bounds
        .extend(self.last_glyphs.iter().map(|glyph| {
            let bounds = &fonts[glyph.font_id.0].glyph_bounds(&glyph.glyph);
            Rect::new(
                Vec2::new(bounds.min.x, bounds.min.y),
                Vec2::new(bounds.max.x, bounds.max.y),
            )
        }));

    // Queue the glyphs for drawing
    self.brush.queue_custom_layout(s, &layout);
}*/
