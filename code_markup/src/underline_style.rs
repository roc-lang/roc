use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::colors::{from_hsb, RgbaTup};

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug, Deserialize, Serialize)]
pub enum UnderlineStyle {
    Error,
    Warning,
}

pub fn default_underline_color_map() -> HashMap<UnderlineStyle, RgbaTup> {
    let mut underline_colors = HashMap::new();

    underline_colors.insert(UnderlineStyle::Error, from_hsb(0, 50, 75));
    underline_colors.insert(UnderlineStyle::Warning, from_hsb(60, 50, 75));

    underline_colors
}
