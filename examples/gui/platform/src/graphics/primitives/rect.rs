use crate::graphics::colors::Rgba;
use cgmath::Vector2;

#[derive(Debug, Copy, Clone)]
pub struct RectElt {
    pub rect: Rect,
    pub color: Rgba,
    pub border_width: f32,
    pub border_color: Rgba,
}

/// These fields are ordered this way because in Roc, the corresponding stuct is:
///
/// { top : F32, left : F32, width : F32, height : F32 }
///
/// alphabetically, that's { height, left, top, width } - which works out to the same as:
///
/// struct Rect { height: f32, pos: Vector2<f32>, width: f32 }
///
/// ...because Vector2<f32> is a repr(C) struct of { x: f32, y: f32 }
#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub struct Rect {
    pub height: f32,
    pub pos: Vector2<f32>,
    pub width: f32,
}
