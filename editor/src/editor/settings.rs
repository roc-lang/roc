

pub struct Settings {
    pub code_font_size: f32,
}

impl Default for Settings {
    fn default() -> Self {

        Self {
            code_font_size: 30.0,
        }
    }
}