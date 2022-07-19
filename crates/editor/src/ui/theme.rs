use gr_colors::{from_hsb, from_hsba, RgbaTup};
use serde::{Deserialize, Serialize};

use crate::graphics::colors as gr_colors;

pub const LIGHT_BRAND_COL: RgbaTup = (0.506, 0.337, 0.902, 1.0); // #8257e5 hsb(258, 62, 90)
pub const DARK_BRAND_COL: RgbaTup = (0.380, 0.169, 0.871, 1.0); // #612bde hsb(258, 81, 87)

#[derive(Deserialize, Serialize)]
pub struct UITheme {
    pub light_brand: RgbaTup,
    pub dark_brand: RgbaTup,
    pub text: RgbaTup,
    pub caret: RgbaTup,
    pub select_highlight: RgbaTup,
    pub tooltip_bg: RgbaTup,
    pub tooltip_text: RgbaTup,
    pub default_font_size: f32,
}

impl Default for UITheme {
    fn default() -> Self {
        Self {
            light_brand: LIGHT_BRAND_COL,
            dark_brand: DARK_BRAND_COL,
            text: gr_colors::WHITE,
            caret: gr_colors::WHITE,
            select_highlight: from_hsba(240, 55, 100, 0.3),
            tooltip_bg: from_hsb(240, 60, 50),
            tooltip_text: gr_colors::WHITE,
            default_font_size: 30.0,
        }
    }
}
