use crate::editor::{ed_error::EdResult, syntax_highlight::HighlightStyle, util::map_get};
use crate::lang::pool::PoolStr;

use crate::graphics::colors::RgbaTup;
use crate::graphics::primitives::text as gr_text;
use bumpalo::collections::String as BumpString;
use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use cgmath::Vector2;
use std::collections::HashMap;
use wgpu_glyph::GlyphBrush;
use winit::dpi::PhysicalSize;

use crate::{
    editor::config::Config,
    graphics::colors,
    lang::{ast::Expr2, expr::Env},
};

fn get_bump_str<'a, 'b>(arena: &'a Bump, env: &Env<'b>, pool_str: &PoolStr) -> BumpString<'a> {
    let env_str = pool_str.as_str(env.pool);

    BumpString::from_str_in(env_str, arena)
}

pub fn highlight_expr2<'a, 'b>(
    arena: &'a Bump,
    env: &Env<'b>,
    expr2: &Expr2,
) -> BumpVec<'a, (BumpString<'a>, HighlightStyle)> {
    let mut highlight_tups: BumpVec<(BumpString<'a>, HighlightStyle)> = BumpVec::new_in(arena);

    let bump_str = BumpString::from_str_in;

    match expr2 {
        Expr2::SmallInt { text, .. }
        | Expr2::I128 { text, .. }
        | Expr2::U128 { text, .. }
        | Expr2::Float { text, .. } => {
            highlight_tups.push((get_bump_str(arena, env, text), HighlightStyle::Number))
        }
        Expr2::Str(text) => {
            let env_str = text.as_str(env.pool);

            highlight_tups.push((
                BumpString::from_str_in(&("\"".to_owned() + env_str + "\""), arena),
                HighlightStyle::String,
            ))
        }
        Expr2::GlobalTag { name, .. } =>
        // TODO split this string up for the brackets
        {
            highlight_tups.push((get_bump_str(arena, env, name), HighlightStyle::Type))
        }
        Expr2::Call { expr: expr_id, .. } => {
            let expr = env.pool.get(*expr_id);
            highlight_tups.append(&mut highlight_expr2(arena, env, expr))
        }
        Expr2::Var(symbol) => {
            //TODO make bump_format with arena
            let text = format!("{:?}", symbol);

            highlight_tups.push((bump_str(&text, arena), HighlightStyle::Variable))
        }
        Expr2::List { elems, .. } => {
            highlight_tups.push((bump_str("[ ", arena), HighlightStyle::Bracket));

            for (idx, node_id) in elems.iter_node_ids().enumerate() {
                let sub_expr2 = env.pool.get(node_id);

                highlight_tups.append(&mut highlight_expr2(arena, env, sub_expr2));

                if idx + 1 < elems.len() {
                    highlight_tups.push((bump_str(", ", arena), HighlightStyle::Operator));
                }
            }

            highlight_tups.push((bump_str(" ]", arena), HighlightStyle::Bracket));
        }
        Expr2::Record { fields, .. } => {
            highlight_tups.push((bump_str("{ ", arena), HighlightStyle::Bracket));

            for (idx, node_id) in fields.iter_node_ids().enumerate() {
                let (pool_field_name, _, sub_expr2_node_id) = env.pool.get(node_id);

                let field_name = pool_field_name.as_str(env.pool);

                let sub_expr2 = env.pool.get(*sub_expr2_node_id);

                highlight_tups.push((bump_str(field_name, arena), HighlightStyle::RecordField));

                highlight_tups.push((bump_str(": ", arena), HighlightStyle::Operator));

                highlight_tups.append(&mut highlight_expr2(arena, env, sub_expr2));

                if idx + 1 < fields.len() {
                    highlight_tups.push((bump_str(", ", arena), HighlightStyle::Operator));
                }
            }

            highlight_tups.push((bump_str(" }", arena), HighlightStyle::Bracket));
        }
        rest => todo!("implement expr2_to_str for {:?}", rest),
    };

    highlight_tups
}

pub fn render_expr2<'a>(
    arena: &'a Bump,
    env: &mut Env<'a>,
    expr2: &Expr2,
    size: &PhysicalSize<u32>,
    position: Vector2<f32>,
    config: &Config,
    glyph_brush: &mut GlyphBrush<()>,
) -> EdResult<()> {
    // TODO formatting code
    let highlight_tups = highlight_expr2(arena, env, expr2);

    queue_code_text_draw(&highlight_tups, size, position, config, glyph_brush)
}

pub fn queue_code_text_draw<'a>(
    highlight_tups: &BumpVec<'a, (BumpString<'a>, HighlightStyle)>,
    size: &PhysicalSize<u32>,
    position: Vector2<f32>,
    config: &Config,
    glyph_brush: &mut GlyphBrush<()>,
) -> EdResult<()> {
    let area_bounds = (size.width as f32, size.height as f32);
    let layout = wgpu_glyph::Layout::default().h_align(wgpu_glyph::HorizontalAlign::Left);

    let glyph_text_vec = highlight_tups_to_glyph_text(
        &highlight_tups,
        &config.ed_theme.syntax_high_map,
        config.code_font_size,
    )?;

    let section =
        gr_text::section_from_glyph_text(glyph_text_vec, position.into(), area_bounds, layout);

    glyph_brush.queue(section.clone());

    Ok(())
}

fn highlight_tups_to_glyph_text<'a>(
    highlight_tups: &'a BumpVec<'a, (BumpString<'a>, HighlightStyle)>,
    syntax_theme: &HashMap<HighlightStyle, RgbaTup>,
    font_size: f32,
) -> EdResult<Vec<wgpu_glyph::Text<'a>>> {
    let arena = Bump::new();
    let mut colored_str_tups: BumpVec<(&BumpString, &RgbaTup)> = BumpVec::new_in(&arena);

    for (token_str, highlight_style) in highlight_tups.iter() {
        let highlight_color_res = map_get(&syntax_theme, highlight_style);

        match highlight_color_res {
            Ok(highlight_color) => colored_str_tups.push((token_str, highlight_color)),
            Err(e) => return Err(e),
        }
    }

    Ok(colored_str_tups
        .iter()
        .map(|(token_str, highlight_color)| {
            wgpu_glyph::Text::new(token_str)
                .with_color(colors::to_slice(**highlight_color))
                .with_scale(font_size)
        })
        .collect())
}
