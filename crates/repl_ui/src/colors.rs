use roc_reporting::report::{StyleCodes, ANSI_STYLE_CODES, HTML_STYLE_CODES};

const STYLE_CODES: StyleCodes = if cfg!(target_family = "wasm") {
    HTML_STYLE_CODES
} else {
    ANSI_STYLE_CODES
};

pub const BLUE: &str = STYLE_CODES.blue;
pub const PINK: &str = STYLE_CODES.magenta;
pub const GREEN: &str = STYLE_CODES.green;
pub const END_COL: &str = STYLE_CODES.reset;
