use crate::graphics::primitives::rect::Rect;
use crate::graphics::primitives::text as gr_text;
use crate::graphics::primitives::text::Text;
use crate::ui::theme::UITheme;

pub struct ToolTip<'a> {
    pub position_x: f32,
    pub position_y: f32,
    pub text: &'a str,
}

impl<'a> ToolTip<'a> {
    fn make_tooltip_rect(
        &self,
        width: f32,
        height: f32,
        height_padding: f32,
        y_margin: f32,
        ui_theme: &UITheme,
    ) -> Rect {
        Rect {
            top_left_coords: (
                self.position_x,
                self.position_y - (height_padding + y_margin),
            )
                .into(),
            height: height + height_padding,
            width,
            color: ui_theme.tooltip_bg,
        }
    }

    fn make_tooltip_text<'b>(
        &'b self,
        x_offset: f32,
        y_offset: f32,
        y_margin: f32,
        ui_theme: &UITheme,
        code_font_size: f32,
    ) -> Text<'b> {
        Text {
            position: (
                self.position_x + x_offset,
                self.position_y - (y_offset + y_margin),
            )
                .into(),
            color: ui_theme.tooltip_text,
            text: self.text,
            size: code_font_size,
            ..Default::default()
        }
    }

    pub fn render_tooltip(
        &self,
        glyph_dim_rect: &Rect,
        ui_theme: &UITheme,
        code_font_size: f32,
    ) -> (Rect, glyph_brush::OwnedSection) {
        let width_padding = glyph_dim_rect.height / 1.3;
        let height_padding = width_padding / 1.3;

        let text_x_offset = width_padding / 2.0;
        let text_y_offset = height_padding / 2.0;

        let y_margin = glyph_dim_rect.height / 4.0;

        let text = self.make_tooltip_text(
            text_x_offset,
            text_y_offset,
            y_margin,
            ui_theme,
            code_font_size,
        );
        let text_section = gr_text::owned_section_from_text(&text);

        let rect = self.make_tooltip_rect(
            glyph_dim_rect.width * (text.text.len() as f32) + width_padding,
            glyph_dim_rect.height,
            height_padding,
            y_margin,
            ui_theme,
        );

        (rect, text_section)
    }
}
