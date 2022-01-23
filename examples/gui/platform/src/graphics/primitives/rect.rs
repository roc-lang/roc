use cgmath::Vector2;

#[derive(Debug, Copy, Clone)]
pub struct Rect {
    pub top_left_coords: Vector2<f32>,
    pub width: f32,
    pub height: f32,
    pub color: (f32, f32, f32, f32),
}
