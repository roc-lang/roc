use serde::{Deserialize, Serialize};

use crate::editor::theme::EdTheme;

use super::resources::strings::START_TIP;

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

impl Config {
    pub fn make_code_txt_xy(&self) -> (f32, f32) {
        (
            self.code_font_size,
            (START_TIP.matches('\n').count() as f32 + 2.0) * self.code_font_size,
        )
    }
}
