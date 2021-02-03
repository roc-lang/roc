use cgmath::Vector2;
use wgpu_glyph::GlyphBrush;
use winit::dpi::PhysicalSize;

use crate::{
    graphics::{
        colors::CODE_COLOR,
        primitives::text::{queue_code_text_draw, Text},
        style::CODE_FONT_SIZE,
    },
    lang::ast::Expr2,
};

pub fn render_expr2(
    size: &PhysicalSize<u32>,
    expr2: &Expr2,
    position: Vector2<f32>,
    glyph_brush: &mut GlyphBrush<()>,
) {
    use Expr2::*;

    let area_bounds = (size.width as f32, size.height as f32).into();

    match expr2 {
        SmallInt {
            number,
            ..
            // text,
            // style, pretending always decimal for now
            // var,
        } => {
            let code_text = Text {
                position,
                area_bounds,
                color: CODE_COLOR.into(),
                text: format!("{:?}", number),
                size: CODE_FONT_SIZE,
                ..Default::default()
            };

            queue_code_text_draw(&code_text, glyph_brush);
        }
        Float {
            number,
            ..
        } => {
            let code_text = Text {
                position,
                area_bounds,
                color: CODE_COLOR.into(),
                text: format!("{:?}", number),
                size: CODE_FONT_SIZE,
                ..Default::default()
            };

            queue_code_text_draw(&code_text, glyph_brush);
        }
        rest => panic!("implement {:?} render", rest)
    };
}
