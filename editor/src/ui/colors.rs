use crate::graphics::colors as gr_colors;
use gr_colors::{RgbaTup, from_hsb};

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
            light_brand: from_hsb(258, 62, 90),
            dark_brand: from_hsb(258, 81, 87),
            text: gr_colors::WHITE,
            caret: gr_colors::WHITE,
            select_highlight: from_hsb(240, 55, 100),
        }
    }
}
