use crate::graphics::colors as gr_colors;
use gr_colors::{from_hsb, RgbaTup};


pub const LIGHT_BRAND_COL: RgbaTup = (0.506, 0.337, 0.902, 1.0); // #8257e5 hsb(258, 62, 90)
pub const DARK_BRAND_COL: RgbaTup = (0.380, 0.169, 0.871, 1.0); // #612bde hsb(258, 81, 87)

pub struct UITheme {
    pub light_brand: RgbaTup,
    pub dark_brand: RgbaTup,
    pub text: RgbaTup,
    pub caret: RgbaTup,
    pub select_highlight: RgbaTup,
}

impl Default for UITheme {
    fn default() -> Self {
        Self {
            light_brand: LIGHT_BRAND_COL,
            dark_brand: DARK_BRAND_COL,
            text: gr_colors::WHITE,
            caret: gr_colors::WHITE,
            select_highlight: from_hsb(240, 55, 100),
        }
    }
}
