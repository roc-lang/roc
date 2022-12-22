#![no_main]
use bumpalo::Bump;
use libfuzzer_sys::fuzz_target;
use roc_parse::test_helpers::parse_expr_with;
use roc_fmt::spaces::RemoveSpaces;
use roc_fmt::annotation::{Formattable, Parens, Newlines};
use roc_fmt::Buf;
use roc_fmt::test_helpers::expr_formats;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        fn expr_formats_to(input: &str, expected: &str) {
            expr_formats(input, |_| (), false);
        }
    }
});
