use cgmath::Vector2;
use wgpu_glyph::GlyphBrush;
use winit::dpi::PhysicalSize;

use crate::{
    graphics::{
        colors::CODE_COLOR,
        primitives::text::{queue_code_text_draw, Text},
        style::CODE_FONT_SIZE,
    },
    lang::{ast::Expr2, expr::Env},
};

pub fn render_expr2<'a>(
    env: &mut Env<'a>,
    size: &PhysicalSize<u32>,
    expr2: &Expr2,
    position: Vector2<f32>,
    glyph_brush: &mut GlyphBrush<()>,
) {
    let area_bounds = (size.width as f32, size.height as f32).into();

    match expr2 {
        Expr2::SmallInt { text, .. } => {
            let code_text = Text {
                position,
                area_bounds,
                color: CODE_COLOR.into(),
                text: env.pool.get_str(text),
                size: CODE_FONT_SIZE,
                ..Default::default()
            };

            queue_code_text_draw(&code_text, glyph_brush);
        }
        Expr2::Float { text, .. } => {
            let code_text = Text {
                position,
                area_bounds,
                color: CODE_COLOR.into(),
                text: env.pool.get_str(text),
                size: CODE_FONT_SIZE,
                ..Default::default()
            };

            queue_code_text_draw(&code_text, glyph_brush);
        }
        rest => todo!("implement {:?} render", rest),
    };
}
