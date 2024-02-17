use roc_reporting::report::{StyleCodes, ANSI_STYLE_CODES, HTML_STYLE_CODES};

const STYLE_CODES: StyleCodes = if cfg!(target_family = "wasm") {
    HTML_STYLE_CODES
} else {
    ANSI_STYLE_CODES
};

pub const GREEN: &str = STYLE_CODES.green;
pub const CYAN: &str = STYLE_CODES.cyan;
pub const END_COL: &str = STYLE_CODES.reset;
