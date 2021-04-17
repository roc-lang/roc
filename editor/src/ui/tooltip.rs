use crate::graphics::primitives::rect::Rect;
use crate::graphics::primitives::text as gr_text;
use crate::graphics::primitives::text::layout_from_text;
use crate::graphics::primitives::text::Text;
use crate::ui::theme::UITheme;

struct ToolTip {
    position_x: f32,
    position_y: f32,
    text: String,
}

impl ToolTip {
    fn make_tooltip_rect(&self, width: f32, height: f32, ui_theme: &UITheme) -> Rect {
        Rect {
            top_left_coords: (self.position_x, self.position_y).into(),
            height,
            width,
            color: ui_theme.tooltip_bg,
        }
    }

    fn make_tooltip_text<'a>(&'a self, ui_theme: &UITheme) -> Text<'a> {
        Text {
            position: (self.position_x, self.position_y).into(), //TODO adjust position
            color: ui_theme.tooltip_text,
            text: &self.text,
            size: ui_theme.default_font_size,
            ..Default::default()
        }
    }

    pub fn render_tooltip(
        &self,
        glyph_dim_rect: &Rect,
        ui_theme: &UITheme,
    ) -> (glyph_brush::OwnedSection, Rect) {
        let text = self.make_tooltip_text(ui_theme);
        let text_layout = layout_from_text(&text);
        let text_section = gr_text::owned_section_from_text(&text, text_layout);

        let padding = 0.5 * glyph_dim_rect.height;
        let rect = self.make_tooltip_rect(
            glyph_dim_rect.width * (text.text.len() as f32) + padding,
            glyph_dim_rect.height + padding,
            ui_theme,
        );

        (text_section, rect)
    }
}
