#[cfg(target_family = "wasm")]
use roc_reporting::report::{StyleCodes, HTML_STYLE_CODES};

#[cfg(not(target_family = "wasm"))]
use roc_reporting::report::{StyleCodes, ANSI_STYLE_CODES};

#[cfg(target_family = "wasm")]
const STYLE_CODES: StyleCodes = HTML_STYLE_CODES;

#[cfg(not(target_family = "wasm"))]
const STYLE_CODES: StyleCodes = ANSI_STYLE_CODES;

pub const BLUE: &str = STYLE_CODES.blue;
pub const PINK: &str = STYLE_CODES.magenta;
pub const GREEN: &str = STYLE_CODES.green;
pub const END_COL: &str = STYLE_CODES.reset;
