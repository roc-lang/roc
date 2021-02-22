use bumpalo::Bump;
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

// TODO use arena allocation
pub fn expr2_to_str<'a>(arena: &'a Bump, env: &Env<'a>, expr2: &Expr2) -> String {
    match expr2 {
        Expr2::SmallInt { text, .. } => env.pool.get_str(text).to_owned(),
        Expr2::I128 { text, .. } => env.pool.get_str(text).to_owned(),
        Expr2::U128 { text, .. } => env.pool.get_str(text).to_owned(),
        Expr2::Float { text, .. } => env.pool.get_str(text).to_owned(),
        Expr2::Str(text) => env.pool.get_str(text).to_owned(),
        Expr2::GlobalTag { name, .. } => env.pool.get_str(name).to_owned(),
        Expr2::Call { expr: expr_id, .. } => {
            let expr = env.pool.get(*expr_id);

            expr2_to_str(arena, env, expr)
        }
        Expr2::Var(symbol) => {
            let text = format!("{:?}", symbol);

            text
        }
        Expr2::List { elems, .. } => {
            let mut list_str = "[".to_owned();

            for (idx, node_id) in elems.iter_node_ids().enumerate() {
                let sub_expr2 = env.pool.get(node_id);

                list_str.push_str(&expr2_to_str(arena, env, sub_expr2));

                if idx + 1 < elems.len() {
                    list_str.push_str(", ")
                }
            }

            list_str.push(']');

            list_str
        }
        Expr2::Record { fields, .. } => {
            let mut record_str = "{".to_owned();

            for (idx, node_id) in fields.iter_node_ids().enumerate() {
                let (pool_field_name, _, sub_expr2_node_id) = env.pool.get(node_id);

                let field_name = env.pool.get_str(pool_field_name);
                let sub_expr2 = env.pool.get(*sub_expr2_node_id);

                record_str.push_str(&format!(
                    "{}:{}",
                    field_name,
                    &expr2_to_str(arena, env, sub_expr2)
                ));

                if idx + 1 < fields.len() {
                    record_str.push_str(", ")
                }
            }

            record_str.push('}');

            record_str
        }
        rest => todo!("implement expr2_to_str for {:?}", rest),
    }
}

pub fn render_expr2<'a>(
    arena: &'a Bump,
    env: &mut Env<'a>,
    expr2: &Expr2,
    size: &PhysicalSize<u32>,
    position: Vector2<f32>,
    glyph_brush: &mut GlyphBrush<()>,
) {
    let area_bounds = (size.width as f32, size.height as f32).into();

    let expr_str = expr2_to_str(arena, env, expr2);

    // TODO format expr_str

    let code_text = Text {
        position,
        area_bounds,
        color: CODE_COLOR.into(),
        text: &expr_str,
        size: CODE_FONT_SIZE,
        ..Default::default()
    };

    queue_code_text_draw(&code_text, glyph_brush);
}
