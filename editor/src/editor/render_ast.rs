use crate::lang::pool::PoolStr;
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use cgmath::Vector2;
use wgpu_glyph::GlyphBrush;
use winit::dpi::PhysicalSize;

use crate::{
    editor::colors::CODE_COL,
    graphics::{
        primitives::text::{queue_code_text_draw, Text},
        style::CODE_FONT_SIZE,
    },
    lang::{ast::Expr2, expr::Env},
};

// calculate the str len, necessary for BumpString
fn expr2_to_len<'a>(env: &Env<'a>, expr2: &Expr2) -> usize {
    match expr2 {
        Expr2::SmallInt { text, .. } => text.len(env.pool),
        Expr2::I128 { text, .. } => text.len(env.pool),
        Expr2::U128 { text, .. } => text.len(env.pool),
        Expr2::Float { text, .. } => text.len(env.pool),
        Expr2::Str(text) => text.len(env.pool),
        Expr2::GlobalTag { name, .. } => name.len(env.pool),
        Expr2::Call { expr: expr_id, .. } => {
            let expr = env.pool.get(*expr_id);

            expr2_to_len(env, expr)
        }
        Expr2::Var(symbol) => {
            //TODO make bump_format to use arena
            let text = format!("{:?}", symbol);

            text.len()
        }
        Expr2::List { elems, .. } => {
            let mut len_ctr = 2; // for '[' and ']'

            for (idx, node_id) in elems.iter_node_ids().enumerate() {
                let sub_expr2 = env.pool.get(node_id);

                len_ctr += expr2_to_len(env, sub_expr2);

                if idx + 1 < elems.len() {
                    len_ctr += 2; // for ", "
                }
            }

            len_ctr
        }
        Expr2::Record { fields, .. } => {
            let mut len_ctr = 2; // for '{' and '}'

            for (idx, node_id) in fields.iter_node_ids().enumerate() {
                let (pool_field_name, _, sub_expr2_node_id) = env.pool.get(node_id);

                len_ctr += pool_field_name.len(env.pool);

                let sub_expr2 = env.pool.get(*sub_expr2_node_id);
                let sub_expr2_len = expr2_to_len(env, sub_expr2);
                len_ctr += sub_expr2_len;

                if idx + 1 < fields.len() {
                    len_ctr += 2; // for ", "
                }
            }

            len_ctr
        }
        rest => todo!("implement expr2_to_str for {:?}", rest),
    }
}

fn get_bump_str<'a, 'b>(arena: &'a Bump, env: &Env<'b>, pool_str: &PoolStr) -> BumpString<'a> {
    let env_str = pool_str.as_str(env.pool);

    BumpString::from_str_in(env_str, arena)
}

pub fn expr2_to_str<'a, 'b>(arena: &'a Bump, env: &Env<'b>, expr2: &Expr2) -> BumpString<'a> {
    match expr2 {
        Expr2::SmallInt { text, .. } => get_bump_str(arena, env, text),
        Expr2::I128 { text, .. } => get_bump_str(arena, env, text),
        Expr2::U128 { text, .. } => get_bump_str(arena, env, text),
        Expr2::Float { text, .. } => get_bump_str(arena, env, text),
        Expr2::Str(text) => get_bump_str(arena, env, text),
        Expr2::GlobalTag { name, .. } => get_bump_str(arena, env, name),
        Expr2::Call { expr: expr_id, .. } => {
            let expr = env.pool.get(*expr_id);

            expr2_to_str(arena, env, expr)
        }
        Expr2::Var(symbol) => {
            //TODO make bump_format with arena
            let text = format!("{:?}", symbol);

            BumpString::from_str_in(&text, arena)
        }
        Expr2::List { elems, .. } => {
            let mut bump_str = BumpString::with_capacity_in(expr2_to_len(env, expr2), arena);

            bump_str.push('[');

            for (idx, node_id) in elems.iter_node_ids().enumerate() {
                let sub_expr2 = env.pool.get(node_id);

                bump_str.push_str(&expr2_to_str(arena, env, sub_expr2));

                if idx + 1 < elems.len() {
                    bump_str.push_str(", ")
                }
            }

            bump_str.push(']');

            bump_str
        }
        Expr2::Record { fields, .. } => {
            let mut bump_str = BumpString::with_capacity_in(expr2_to_len(env, expr2), arena);

            bump_str.push('{');

            for (idx, node_id) in fields.iter_node_ids().enumerate() {
                let (pool_field_name, _, sub_expr2_node_id) = env.pool.get(node_id);

                let field_name = pool_field_name.as_str(env.pool);

                let sub_expr2 = env.pool.get(*sub_expr2_node_id);

                bump_str.push_str(field_name);
                bump_str.push(':');
                bump_str.push_str(&expr2_to_str(arena, env, sub_expr2));

                if idx + 1 < fields.len() {
                    bump_str.push_str(", ")
                }
            }

            bump_str.push('}');

            bump_str
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
        color: CODE_COL.into(),
        text: &expr_str,
        size: CODE_FONT_SIZE,
        ..Default::default()
    };

    queue_code_text_draw(&code_text, glyph_brush);
}
