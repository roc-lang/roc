use crate::editor::theme::EdTheme;

pub struct Config {
    pub code_font_size: f32,
    pub ed_theme: EdTheme,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            code_font_size: 30.0,
            ed_theme: EdTheme::default(),
        }
    }
}
