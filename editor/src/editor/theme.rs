use gr_colors::{from_hsb, RgbaTup};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::editor::syntax_highlight::{default_highlight_map, HighlightStyle};
use crate::graphics::colors as gr_colors;
use crate::ui::theme::UITheme;

#[derive(Serialize, Deserialize)]
pub struct EdTheme {
    pub background: RgbaTup,
    pub syntax_high_map: HashMap<HighlightStyle, RgbaTup>,
    pub ui_theme: UITheme,
}

impl Default for EdTheme {
    fn default() -> Self {
        Self {
            background: from_hsb(240, 10, 19), // #2C2C35
            syntax_high_map: default_highlight_map(),
            ui_theme: UITheme::default(),
        }
    }
}
