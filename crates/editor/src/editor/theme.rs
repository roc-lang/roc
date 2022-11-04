use gr_colors::{from_hsb, RgbaTup};
use roc_code_markup::{
    syntax_highlight::{default_highlight_map, HighlightStyle},
    underline_style::{default_underline_color_map, UnderlineStyle},
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::graphics::colors as gr_colors;
use crate::ui::theme::UITheme;

#[derive(Serialize, Deserialize)]
pub struct EdTheme {
    pub background: RgbaTup,
    pub subtle_text: RgbaTup,
    pub syntax_high_map: HashMap<HighlightStyle, RgbaTup>,
    pub ui_theme: UITheme,
    pub underline_color_map: HashMap<UnderlineStyle, RgbaTup>,
}

impl Default for EdTheme {
    fn default() -> Self {
        Self {
            background: from_hsb(240, 10, 19), // #2C2C35
            subtle_text: from_hsb(240, 5, 60),
            syntax_high_map: default_highlight_map(),
            ui_theme: UITheme::default(),
            underline_color_map: default_underline_color_map(),
        }
    }
}
