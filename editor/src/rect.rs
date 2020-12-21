use cgmath::Vector2;
use wgpu_glyph::{ab_glyph};

pub struct Rect {
    pub top_left_coords: Vector2<f32>,
    pub width: f32,
    pub height: f32,
    pub color: [f32; 3],
}

pub fn convert_rect(glyph_rect: ab_glyph::Rect, color: [f32; 3]) -> Rect {
    Rect {
        top_left_coords: [glyph_rect.min.x, glyph_rect.min.y].into(),
        width: glyph_rect.width(),
        height: glyph_rect.height(),
        color
    }
} 
