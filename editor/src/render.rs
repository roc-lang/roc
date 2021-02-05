use bumpalo::{collections, Bump};
use cgmath::Vector2;
use wgpu_glyph::GlyphBrush;
use winit::dpi::PhysicalSize;

use crate::{
    graphics::{
        colors::CODE_COLOR,
        primitives::text::{queue_code_text_draw, Text},
        style::CODE_FONT_SIZE,
    },
    lang::ast::{Expr2, FloatVal, IntVal},
};

pub fn render_expr2<'a>(
    arena: &'a Bump,
    size: &PhysicalSize<u32>,
    expr2: &Expr2,
    position: Vector2<f32>,
    glyph_brush: &mut GlyphBrush<()>,
) {
    use std::fmt::Write;

    let area_bounds = (size.width as f32, size.height as f32).into();

    match expr2 {
        Expr2::SmallInt {
            number,
            ..
            // text,
            // style, pretending always decimal for now
            // var,
        } => {
            let mut text = collections::String::with_capacity_in(40, arena);

            let _ = match number {
                IntVal::I64(val) => write!(text, "{}", val),
                IntVal::I32(val) => write!(text, "{}", val),
                IntVal::I16(val) => write!(text, "{}", val),
                IntVal::I8(val) => write!(text, "{}", val),
                IntVal::U64(val) => write!(text, "{}", val),
                IntVal::U32(val ) => write!(text, "{}", val),
                IntVal::U16(val) => write!(text, "{}", val),
                IntVal::U8(val) => write!(text, "{}", val),
            };

            let code_text = Text {
                position,
                area_bounds,
                color: CODE_COLOR.into(),
                text: text.as_str(),
                size: CODE_FONT_SIZE,
                ..Default::default()
            };

            queue_code_text_draw(&code_text, glyph_brush);
        }
        Expr2::Float {
            number,
            ..
        } => {
            let mut text = collections::String::with_capacity_in(40, arena);

            let _ = match number {
                FloatVal::F64(val) => write!(text, "{}", val),
                FloatVal::F32(val) => write!(text, "{}", val),
            };

            let code_text = Text {
                position,
                area_bounds,
                color: CODE_COLOR.into(),
                text: text.as_str(),
                size: CODE_FONT_SIZE,
                ..Default::default()
            };

            queue_code_text_draw(&code_text, glyph_brush);
        }
        rest => todo!("implement {:?} render", rest)
    };
}
