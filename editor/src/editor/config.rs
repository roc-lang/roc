use serde::{Deserialize, Serialize};

use crate::editor::theme::EdTheme;

#[derive(Serialize, Deserialize)]
pub struct Config {
    pub code_font_size: f32,
    pub debug_font_size: f32,
    pub ed_theme: EdTheme,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            code_font_size: 30.0,
            debug_font_size: 20.0,
            ed_theme: EdTheme::default(),
        }
    }
}
