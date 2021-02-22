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
    expr2: &Expr2,
    size: &PhysicalSize<u32>,
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
        Expr2::I128 { text, .. } => {
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
        Expr2::U128 { text, .. } => {
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
        Expr2::Str(text) => {
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
        Expr2::GlobalTag { name, .. } => {
            let code_text = Text {
                position,
                area_bounds,
                color: CODE_COLOR.into(),
                text: env.pool.get_str(name),
                size: CODE_FONT_SIZE,
                ..Default::default()
            };

            queue_code_text_draw(&code_text, glyph_brush);
        }
        Expr2::Call { expr: expr_id, .. } => {
            let expr = env.pool.get(*expr_id);

            render_expr2(env, expr, size, position, glyph_brush);
        }
        Expr2::Var(symbol) => {
            let text = format!("{:?}", symbol);

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
        Expr2::List { elems, .. } => {
            let code_text = Text {
                position,
                area_bounds,
                color: CODE_COLOR.into(),
                text: "[",
                size: CODE_FONT_SIZE,
                ..Default::default()
            };

            queue_code_text_draw(&code_text, glyph_brush);

            let mut x_pos = position.x;

            for (idx, node_id) in elems.iter_node_ids().enumerate() {
                let sub_expr2 = env.pool.get(node_id);

                x_pos += 20.0;

                render_expr2(
                    env,
                    sub_expr2,
                    size,
                    Vector2::new(x_pos, position.y),
                    glyph_brush,
                );

                if idx + 1 < elems.len() {
                    x_pos += 10.0;

                    let code_text = Text {
                        position: Vector2::new(x_pos, position.y),
                        area_bounds,
                        color: CODE_COLOR.into(),
                        text: ",",
                        size: CODE_FONT_SIZE,
                        ..Default::default()
                    };

                    queue_code_text_draw(&code_text, glyph_brush);
                }
            }

            x_pos += 20.0;

            let code_text = Text {
                position: Vector2::new(x_pos, position.y),
                area_bounds,
                color: CODE_COLOR.into(),
                text: "]",
                size: CODE_FONT_SIZE,
                ..Default::default()
            };

            queue_code_text_draw(&code_text, glyph_brush);
        }
        Expr2::Record { fields, .. } => {
            let code_text = Text {
                position,
                area_bounds,
                color: CODE_COLOR.into(),
                text: "{",
                size: CODE_FONT_SIZE,
                ..Default::default()
            };

            queue_code_text_draw(&code_text, glyph_brush);

            let mut x_pos = position.x;

            for (idx, node_id) in fields.iter_node_ids().enumerate() {
                let (pool_field_name, _, sub_expr2_node_id) = env.pool.get(node_id);

                let field_name = env.pool.get_str(pool_field_name);

                x_pos += 20.0;

                let code_text = Text {
                    position: Vector2::new(x_pos, position.y),
                    area_bounds,
                    color: CODE_COLOR.into(),
                    text: field_name,
                    size: CODE_FONT_SIZE,
                    ..Default::default()
                };

                queue_code_text_draw(&code_text, glyph_brush);

                x_pos += 10.0;

                let code_text = Text {
                    position: Vector2::new(x_pos, position.y),
                    area_bounds,
                    color: CODE_COLOR.into(),
                    text: ":",
                    size: CODE_FONT_SIZE,
                    ..Default::default()
                };

                queue_code_text_draw(&code_text, glyph_brush);

                let sub_expr2 = env.pool.get(*sub_expr2_node_id);

                x_pos += 20.0;

                render_expr2(
                    env,
                    sub_expr2,
                    size,
                    Vector2::new(x_pos, position.y),
                    glyph_brush,
                );

                if idx + 1 < fields.len() {
                    x_pos += 10.0;

                    let code_text = Text {
                        position: Vector2::new(x_pos, position.y),
                        area_bounds,
                        color: CODE_COLOR.into(),
                        text: ",",
                        size: CODE_FONT_SIZE,
                        ..Default::default()
                    };

                    queue_code_text_draw(&code_text, glyph_brush);
                }
            }

            x_pos += 20.0;

            let code_text = Text {
                position: Vector2::new(x_pos, position.y),
                area_bounds,
                color: CODE_COLOR.into(),
                text: "}",
                size: CODE_FONT_SIZE,
                ..Default::default()
            };

            queue_code_text_draw(&code_text, glyph_brush);
        }
        rest => todo!("implement {:?} render", rest),
    };
}
