use crate::graphics::colors as gr_colors;
use gr_colors::{from_hsb, RgbaTup};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug, Deserialize, Serialize)]
pub enum HighlightStyle {
    Operator, // =+-<>...
    String,
    FunctionName,
    Type,
    Bracket,
    Number,
    PackageRelated, // app, packages, imports, exposes, provides...
    Variable,
    RecordField,
    Import,
    Provides,
    Blank,
}

pub fn default_highlight_map() -> HashMap<HighlightStyle, RgbaTup> {
    use HighlightStyle::*;

    let mut highlight_map = HashMap::new();
    [
        (Operator, gr_colors::WHITE),
        (String, from_hsb(346, 65, 97)),
        (FunctionName, gr_colors::WHITE),
        (Type, gr_colors::WHITE),
        (Bracket, from_hsb(347, 80, 100)),
        (Number, from_hsb(185, 50, 75)),
        (PackageRelated, gr_colors::WHITE),
        (Variable, gr_colors::WHITE),
        (RecordField, from_hsb(258, 50, 90)),
        (Import, from_hsb(185, 50, 75)),
        (Provides, from_hsb(185, 50, 75)),
        (Blank, from_hsb(258, 50, 90)),
        // comment from_hsb(285, 6, 47) or 186, 35, 40
    ]
    .iter()
    .for_each(|tup| {
        highlight_map.insert(tup.0, tup.1);
    });

    highlight_map
}
