// Adapted from https://github.com/sotrh/learn-wgpu
// by Benjamin Hansen, licensed under the MIT license

use super::rect::Rect;
use crate::editor::syntax_highlight;
use crate::graphics::colors;
use crate::graphics::colors::RgbaTup;
use crate::graphics::style::{CODE_TXT_XY, DEFAULT_FONT_SIZE};
use ab_glyph::{FontArc, Glyph, InvalidFont};
use cgmath::{Vector2, Vector4};
use wgpu_glyph::{ab_glyph, GlyphBrush, GlyphBrushBuilder, GlyphCruncher, Section};

#[derive(Debug)]
pub struct Text<'a> {
    pub position: Vector2<f32>,
    pub area_bounds: Vector2<f32>,
    pub color: RgbaTup,
    pub text: &'a str,
    pub size: f32,
    pub visible: bool,
    pub centered: bool,
}

impl<'a> Default for Text<'a> {
    fn default() -> Self {
        Self {
            position: (0.0, 0.0).into(),
            area_bounds: (std::f32::INFINITY, std::f32::INFINITY).into(),
            color: colors::WHITE,
            text: "",
            size: DEFAULT_FONT_SIZE,
            visible: true,
            centered: false,
        }
    }
}

// necessary to get dimensions for caret
pub fn example_code_glyph_rect(glyph_brush: &mut GlyphBrush<()>) -> Rect {
    let code_text = Text {
        position: CODE_TXT_XY.into(),
        area_bounds: (std::f32::INFINITY, std::f32::INFINITY).into(),
        color: colors::WHITE,
        text: "a",
        size: DEFAULT_FONT_SIZE,
        ..Default::default()
    };

    let layout = layout_from_text(&code_text);

    let section = section_from_text(&code_text, layout);

    let mut glyph_section_iter = glyph_brush.glyphs_custom_layout(section, &layout);

    if let Some(glyph) = glyph_section_iter.next() {
        glyph_to_rect(glyph)
    } else {
        unreachable!();
    }
}

fn layout_from_text(text: &Text) -> wgpu_glyph::Layout<wgpu_glyph::BuiltInLineBreaker> {
    wgpu_glyph::Layout::default().h_align(if text.centered {
        wgpu_glyph::HorizontalAlign::Center
    } else {
        wgpu_glyph::HorizontalAlign::Left
    })
}

fn section_from_text<'a>(
    text: &'a Text,
    layout: wgpu_glyph::Layout<wgpu_glyph::BuiltInLineBreaker>,
) -> wgpu_glyph::Section<'a> {
    Section {
        screen_position: text.position.into(),
        bounds: text.area_bounds.into(),
        layout,
        ..Section::default()
    }
    .add_text(
        wgpu_glyph::Text::new(&text.text)
            .with_color(Vector4::from(text.color))
            .with_scale(text.size),
    )
}

fn section_from_glyph_text(
    text: Vec<wgpu_glyph::Text>,
    screen_position: (f32, f32),
    area_bounds: (f32, f32),
    layout: wgpu_glyph::Layout<wgpu_glyph::BuiltInLineBreaker>,
) -> wgpu_glyph::Section {
    Section {
        screen_position,
        bounds: area_bounds,
        layout,
        text,
    }
}

fn colored_text_to_glyph_text(text_tups: &[(String, RgbaTup)]) -> Vec<wgpu_glyph::Text> {
    text_tups
        .iter()
        .map(|(word_string, color_tup)| {
            wgpu_glyph::Text::new(&word_string)
                .with_color(colors::to_slice(*color_tup))
                .with_scale(DEFAULT_FONT_SIZE)
        })
        .collect()
}

pub fn queue_text_draw(text: &Text, glyph_brush: &mut GlyphBrush<()>) {
    let layout = layout_from_text(text);

    let section = section_from_text(text, layout);

    glyph_brush.queue(section.clone());
}

// TODO move this out of graphics folder and make syntax_theme an argument
pub fn queue_code_text_draw(text: &Text, glyph_brush: &mut GlyphBrush<()>) {
    let layout = layout_from_text(text);

    let mut all_text_tups: Vec<(String, RgbaTup)> = Vec::new();

    let syntax_theme = crate::editor::colors::SyntaxHighlightTheme::default();
    syntax_highlight::highlight_code(text, &mut all_text_tups, &syntax_theme);
    let glyph_text_vec = colored_text_to_glyph_text(&all_text_tups);

    let section = section_from_glyph_text(
        glyph_text_vec,
        text.position.into(),
        text.area_bounds.into(),
        layout,
    );

    glyph_brush.queue(section.clone());
}

fn glyph_to_rect(glyph: &wgpu_glyph::SectionGlyph) -> Rect {
    let position = glyph.glyph.position;
    let px_scale = glyph.glyph.scale;
    let width = glyph_width(&glyph.glyph);
    let height = px_scale.y;
    let top_y = glyph_top_y(&glyph.glyph);

    Rect {
        top_left_coords: [position.x, top_y].into(),
        width,
        height,
        color: colors::WHITE,
    }
}

pub fn glyph_top_y(glyph: &Glyph) -> f32 {
    let height = glyph.scale.y;

    glyph.position.y - height * 0.75
}

pub fn glyph_width(glyph: &Glyph) -> f32 {
    glyph.scale.x * 0.4765
}

pub fn build_glyph_brush(
    gpu_device: &wgpu::Device,
    render_format: wgpu::TextureFormat,
) -> Result<GlyphBrush<()>, InvalidFont> {
    let inconsolata = FontArc::try_from_slice(include_bytes!("../../../Inconsolata-Regular.ttf"))?;

    Ok(GlyphBrushBuilder::using_font(inconsolata).build(&gpu_device, render_format))
}
