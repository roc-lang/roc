use roc_reporting::report::{StyleCodes, HTML_STYLE_CODES, XTERM_256_COLOR_STYLE_CODES};

const STYLE_CODES: StyleCodes = if cfg!(target_family = "wasm") {
    HTML_STYLE_CODES
} else {
    XTERM_256_COLOR_STYLE_CODES
};

pub const BLUE: &str = STYLE_CODES.blue;
pub const PINK: &str = STYLE_CODES.magenta;
pub const GREEN: &str = STYLE_CODES.green;
pub const END_COL: &str = STYLE_CODES.reset;
