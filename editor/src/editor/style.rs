use crate::editor::config::Config;

pub fn get_code_txt_xy(config: &Config) -> (f32, f32) {
    (config.code_font_size, config.code_font_size * 10.00)
}
