
use crate::graphics::colors as gr_colors;
use gr_colors::{RgbaTup, from_hsb};
use crate::ui::colors as ui_colors;
use ui_colors::UITheme;

pub struct SyntaxHighlightTheme {
    pub code: RgbaTup,
    // operators are "=+-:>..." 
    pub operator: RgbaTup,
    pub string: RgbaTup,
}

impl Default for SyntaxHighlightTheme {
    fn default() -> Self {
        let ui_theme = UITheme::default();

        Self {
            code: gr_colors::WHITE,
            operator: from_hsb(257, 81, 10),
            string: ui_theme.light_brand,
        }
    }
}

pub struct EdTheme {
    pub background: RgbaTup,
    pub syntax_high_theme: SyntaxHighlightTheme, 
    pub ui_theme: UITheme,
}

impl Default for EdTheme {
    fn default() -> Self {

        Self {
            background: from_hsb(240, 10, 19),
            syntax_high_theme: SyntaxHighlightTheme::default(),
            ui_theme: UITheme::default(),
        }
    }
}
